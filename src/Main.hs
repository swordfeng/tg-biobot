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
import Control.Exception (try, SomeException)
import Control.Monad (guard)
import Secret

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
        Left e -> print e >> processUpdates token manager startPos
        Right UpdatesResponse { update_result = updates } -> do
            forM_ updates handleUpdate
            processUpdates token manager (Just . (+1) . maximum . map update_id $ updates)
    where
        handleUpdate Update { message = Just msg } = do
            forM_ textMessageHandlers $ \handler -> try (handler msg) :: IO (Either SomeException ())
        handleUpdate _ = return ()
        textMessageHandlers = [pingHandler, startHandler]
        pingRegex = Regex.mkRegex "^/ping(@swbiobot|)\\b"
        pingHandler msg@(Message { text = Just str }) = do
            guard $ Regex.matchRegex pingRegex (T.unpack str) /= Nothing
            let req = sendMessageRequest (T.pack . show . chat_id . chat $ msg) "pong"
            result <- sendMessage token req manager
            case result of
                Left e -> print e
        startRegex = Regex.mkRegex "^/start(@swbiobot|)\\b"
        startHandler msg@(Message { text = Just str }) = do
            guard $ Regex.matchRegex startRegex (T.unpack str) /= Nothing
            let req = sendMessageRequest (T.pack . show . chat_id . chat $ msg) "Welcome to use BioBot by @swordfeng!"
            result <- sendMessage token req manager
            case result of
                Left e -> print e
