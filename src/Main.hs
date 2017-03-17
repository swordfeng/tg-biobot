{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, guard, when, unless)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot.API
import Web.Telegram.API.Bot.Responses
import Web.Telegram.API.Bot.Data
import Web.Telegram.API.Bot.Requests
import Servant.Common.Req (ServantError)
import qualified Text.Regex as Regex
import qualified Data.Text as T
import Control.Exception (try, SomeException, displayException)
import Data.Maybe (isNothing, isJust, fromMaybe)
import Secret
import DB (initDB, getBio, setBio, getUserState, setUserState, setParseMode)
import qualified Data.List as L
import Data.TCache (atomically, atomicallySync)

data Ctx = Ctx {
    ctxToken :: Token,
    ctxManager :: Manager,
    ctxStartPos :: Maybe Int
}

main = do
    manager <- newManager tlsManagerSettings
    res <- getMe token manager
    case res of
        Right Response { result = u } -> putStrLn "initialized"
        Left e -> fail $ displayException e
    initDB
    processUpdates $ Ctx token manager Nothing

processUpdates :: Ctx -> IO ()
processUpdates ctx@(Ctx token manager startPos) = do
    result <- getUpdates token startPos Nothing (Just 30) manager
    case result of
        Left e -> do unless ("ResponseTimeout" `L.isInfixOf` displayException e) $ print e
                     processUpdates ctx
        Right Response { result = updates } -> do
            forM_ updates handleUpdate
            processUpdates ctx { ctxStartPos = Just . (+1) . maximum . map update_id $ updates }
    where
        handleUpdate update =
            case update of
                Update { message = Just msg } -> forM_ textMessageHandlers $ \handler -> try (handler msg) :: IO (Either SomeException ())
                Update { inline_query = Just inlineQuery } -> inlineQueryHandler inlineQuery
                _ -> return ()
        textMessageHandlers = [helpHandler helpRegex, helpHandler startRegex, bioHandler, setbioHandler]

        helpRegex = Regex.mkRegex "^/help(@swbiobot|)\\b"
        startRegex = Regex.mkRegex "^/start(@swbiobot|)\\b"
        helpHandler regex msg@Message { text = Just str } = do
            guard $ isJust $ Regex.matchRegex regex (T.unpack str)
            let req = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) helpMessage) {
                message_reply_to_message_id = Just $ message_id msg
            }
            perror =<< sendMessage token req manager

        bioRegex = Regex.mkRegex "^/bio(@swbiobot|$|\\s)\\b\\s*@{0,1}(\\S*)"
        bioHandler msg@Message { text = Just str } =
            case Regex.matchRegex bioRegex (T.unpack str) of
                Just [_, username] -> if not $ null username then doGetBio username else do
                    let Just User { user_username = Just username } = from msg
                    doGetBio $ T.unpack username
            where doGetBio username = do
                    queryResult <- getBio username
                    let (retMsg, parsemode) = fromMaybe ("Bio for user \'" ++ username ++ "\' is not set.", "plain") queryResult
                    let req = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) $ T.pack retMsg) {
                        message_reply_to_message_id = Just $ message_id msg,
                        message_parse_mode = parsemodeStrToVal parsemode
                    }
                    perror =<< sendMessage token req manager

        inlineQueryHandler inlineQuery = do
            let username = case T.unpack $ query_query inlineQuery of { '@':n -> n; n -> n; }
            queryResult <- getBio username
            case queryResult of
                Nothing -> answerInlineQuery token (answerInlineQueryRequest (query_id inlineQuery) []) manager
                Just (bio, parsemode) -> answerInlineQuery token (answerInlineQueryRequest (query_id inlineQuery) [resultArticle]) manager
                    where
                        resultArticle = inlineQueryResultArticle (T.pack username) (T.pack username) (InputTextMessageContent (T.pack content) (parsemodeStrToVal parsemode) Nothing)
                        content = "User: " ++ username ++ "\n\n" ++ bio
            return ()


        setbioRegex = Regex.mkRegex "^/setbio(@swbiobot|)\\b"
        setbioHandler msg@Message { text = Just str } = do
            let retMsg str = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) str) {
                message_reply_to_message_id = Just $ message_id msg
            }
            -- check userID == chatID
            let Just User { user_id = uid, user_username = unameM } = from msg
            let cid = chat_id . chat $ msg
            let match = Regex.matchRegex setbioRegex (T.unpack str)
            if isNothing match then do
                state <- getUserState uid
                case state of
                    "setbio" -> when (uid == cid) $ perror =<< do
                        let Just username = unameM
                        atomicallySync $ do
                            setBio uid (T.unpack username) (T.unpack str)
                            setUserState uid "setbiopm"
                        let msg = (retMsg "Now set your bio's parse mode:") {
                            message_reply_markup = Just . replyKeyboardMarkup $ [map keyboardButton ["plain", "markdown", "html"]]
                        }
                        sendMessage token msg manager
                    "setbiopm" -> when (uid == cid) $ perror =<< do
                        let Just username = unameM
                        if str /= "markdown" && str /= "html" && str /= "plain" then do
                            let msg = retMsg "Invalid reply! Please re-input:"
                            sendMessage token msg manager
                        else do
                            atomicallySync $ do
                                setParseMode uid (T.unpack str)
                                setUserState uid ""
                            let msg = (retMsg "Your bio is successfully set.") {
                                message_reply_markup = Just replyKeyboardHide
                            }
                            sendMessage token msg manager
                    _ -> return ()
                else perror =<<
                    if uid /= cid then
                        sendMessage token (retMsg "Set your bio in private chat please!") manager
                    else case unameM of
                        Nothing -> sendMessage token (retMsg "Set your username please!") manager
                        Just username -> do
                            atomicallySync $ setUserState uid "setbio"
                            sendMessage token (retMsg "Now reply to me to set your bio:") manager

helpMessage = T.pack . unlines $ [
    "Welcome to BioBot by @swordfeng!",
    "Project home: https://github.com/swordfeng/tg-biobot",
    "Commands:",
    "    /help - show this help",
    "    /bio [@][username] - show one's bio",
    "    /setbio - set your bio (private chat only)",
    "Inline query:",
    "    @swbiobot [@]username - show one's bio (without this bot in the group)"
    ]

parsemodeStrToVal "markdown" = Just Markdown
parsemodeStrToVal "html" = Just HTML
parsemodeStrToVal _ = Nothing


perror (Left e) = print e
perror _ = return ()
