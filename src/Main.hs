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
import qualified Data.List as L

main = do
    manager <- newManager tlsManagerSettings
    res <- getMe token manager
    case res of
        Right GetMeResponse { user_result = u } -> do
            putStrLn "initialized"
            print $ user_first_name u
        Left e -> print e
    processUpdates token manager Nothing

processUpdates :: Token -> Manager -> Maybe Int -> IO ()
processUpdates token manager startPos = do
    result <- getUpdates token startPos Nothing (Just 30) manager
    case result of
        Left e -> (if L.isInfixOf "ResponseTimeout" (displayException e) then return () else print e)
            >> processUpdates token manager startPos
        Right UpdatesResponse { update_result = updates } -> do
            forM_ updates handleUpdate
            processUpdates token manager (Just . (+1) . maximum . map update_id $ updates)
    where
        handleUpdate Update { message = Just msg } = do
            forM_ textMessageHandlers $ \handler -> try (handler msg) :: IO (Either SomeException ())
        handleUpdate _ = return ()
        textMessageHandlers = [pingHandler, startHandler]
        pingRegex = Regex.mkRegex "^/help(@swbiobot|)\\b"
        pingHandler msg@(Message { text = Just str }) = do
            guard $ Regex.matchRegex pingRegex (T.unpack str) /= Nothing
            let req = messageReq {
                message_chat_id = T.pack . show . chat_id . chat $ msg,
                message_text = helpMessage,
                message_reply_to_message_id = Just $ message_id msg
            }
            result <- sendMessage token req manager
            case result of
                Left e -> print e
        startRegex = Regex.mkRegex "^/start(@swbiobot|)\\b"
        startHandler msg@(Message { text = Just str }) = do
            guard $ Regex.matchRegex startRegex (T.unpack str) /= Nothing
            let req = messageReq {
                message_chat_id = (T.pack . show . chat_id . chat $ msg),
                message_text = helpMessage,
                message_reply_to_message_id = Just $ message_id msg
            }
            result <- sendMessage token req manager
            case result of
                Left e -> print e

helpMessage = T.pack $ L.intercalate "\n" [
    "Welcome to BioBot by @swordfeng!",
    "Commands:",
    "    /help - show this help"
    ]

messageReq = SendMessageRequest {
    message_chat_id = T.pack "",
    message_text = "",
    message_parse_mode = Just Markdown,
    message_disable_web_page_preview = Nothing,
    message_disable_notification = Nothing,
    message_reply_to_message_id = Nothing,
    message_reply_markup = Nothing
}
