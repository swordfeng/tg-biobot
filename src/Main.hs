{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot.API
import Web.Telegram.API.Bot.Responses
import Web.Telegram.API.Bot.Data
import Web.Telegram.API.Bot.Requests
import Network.HTTP.Client (Manager)
import Servant.Common.Req (ServantError)
import qualified Text.Regex as Regex
import qualified Data.Text as T
import Control.Exception (try, SomeException, displayException)
import Control.Monad (guard)
import Secret
import DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Data.List as L

data Ctx = Ctx {
    ctx_token :: Token,
    ctx_manager :: Manager,
    ctx_startPos :: Maybe Int,
    ctx_conn :: Sqlite3.Connection
}

main = do
    manager <- newManager tlsManagerSettings
    res <- getMe token manager
    case res of
        Right GetMeResponse { user_result = u } -> do
            putStrLn "initialized"
        Left e -> fail $ displayException e
    conn <- Sqlite3.connectSqlite3 "data.db"
    initDB conn
    processUpdates $ Ctx token manager Nothing conn

processUpdates :: Ctx -> IO ()
processUpdates ctx@(Ctx token manager startPos conn) = do
    result <- getUpdates token startPos Nothing (Just 30) manager
    case result of
        Left e -> (if L.isInfixOf "ResponseTimeout" (displayException e) then return () else print e)
            >> processUpdates ctx
        Right UpdatesResponse { update_result = updates } -> do
            forM_ updates handleUpdate
            processUpdates ctx { ctx_startPos = Just . (+1) . maximum . map update_id $ updates }
    where
        handleUpdate update = do
            case update of
                Update { message = Just msg } -> forM_ textMessageHandlers $ \handler -> try (handler msg) :: IO (Either SomeException ())
                Update { inline_query = Just inlineQuery } -> inlineQueryHandler inlineQuery
                _ -> return ()
        textMessageHandlers = [helpHandler helpRegex, helpHandler startRegex, bioHandler, setbioHandler]

        helpRegex = Regex.mkRegex "^/help(@swbiobot|)\\b"
        startRegex = Regex.mkRegex "^/start(@swbiobot|)\\b"
        helpHandler regex msg@(Message { text = Just str }) = do
            guard $ Regex.matchRegex regex (T.unpack str) /= Nothing
            let req = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) helpMessage) {
                message_reply_to_message_id = Just $ message_id msg
            }
            result <- sendMessage token req manager
            case result of
                Left e -> print e

        bioRegex = Regex.mkRegex "^/bio(@swbiobot|$|\\s)\\b\\s*(\\S*)"
        bioHandler msg@(Message { text = Just str }) = do
            case Regex.matchRegex bioRegex (T.unpack str) of
                Just [_, username] -> if length username > 0 then doGetBio username else do
                    let Just (User { user_username = Just username }) = from msg
                    doGetBio $ T.unpack username
            where doGetBio username = do
                    queryResult <- getBio conn username
                    let retMsg = case queryResult of
                            Nothing -> "Bio for user \'" ++ username ++ "\' is not set."
                            Just bio -> bio
                    let req = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) $ T.pack retMsg) {
                        message_reply_to_message_id = Just $ message_id msg
                    }
                    result <- sendMessage token req manager
                    case result of
                        Left e -> print e

        inlineQueryHandler inlineQuery = do
            let username = T.unpack $ query_query inlineQuery
            queryResult <- getBio conn username
            case queryResult of
                Nothing -> answerInlineQuery token (answerInlineQueryRequest (query_id inlineQuery) []) manager
                Just bio -> answerInlineQuery token (answerInlineQueryRequest (query_id inlineQuery) [resultArticle]) manager
                    where
                        resultArticle = inlineQueryResultArticle (T.pack username) (T.pack $ "found bio for " ++ username) (InputTextMessageContent (T.pack bio) Nothing Nothing)
            return ()


        setbioRegex = Regex.mkRegex "^/setbio(@swbiobot|)\\b"
        setbioHandler msg@(Message { text = Just str }) = do
            let retMsg str = (sendMessageRequest (T.pack . show . chat_id . chat $ msg) str) {
                message_reply_to_message_id = Just $ message_id msg
            }
            -- check userID == chatID
            let Just (User { user_id = uid, user_username = unameM }) = from msg
            let cid = chat_id . chat $ msg
            let match = Regex.matchRegex setbioRegex (T.unpack str)
            if match == Nothing then do
                state <- getUserState conn uid
                if state /= "setbio" then return ()
                else do
                    result <-
                        if uid /= cid then
                            sendMessage token (retMsg "Set your bio in private chat please!") manager
                        else do
                            let Just username = unameM
                            setBio conn (T.unpack username) (T.unpack str)
                            setUserState conn uid ""
                            sendMessage token (retMsg "Your bio is successfully set." ) manager
                    case result of
                        Left e -> print e
            else do
                result <-
                    if uid /= cid then
                        sendMessage token (retMsg "Set your bio in private chat please!") manager
                    else case unameM of
                        Nothing -> sendMessage token (retMsg "Set your username please!") manager
                        Just username -> do
                            setUserState conn uid "setbio"
                            sendMessage token (retMsg "Now reply to me to set your bio:") manager
                case result of
                    Left e -> print e

helpMessage = T.pack . unlines $ [
    "Welcome to BioBot by @swordfeng!",
    "Project home: https://github.com/swordfeng/tg-biobot",
    "Commands:",
    "    /help - show this help",
    "    /bio \\[username] - show one's bio",
    "    /setbio - set your bio"
    ]
