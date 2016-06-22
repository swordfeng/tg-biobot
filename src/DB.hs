module DB where

import Database.HDBC
import Control.Exception
import Control.Monad (mzero)

initDB conn = do
    _ <- runRaw conn . unlines $ [
        "CREATE TABLE IF NOT EXISTS state (",
        "    uid int64,",
        "    state varchar(15) not null default \'\',",
        "    primary key (uid));"
        ]
    _ <- runRaw conn . unlines $ [
        "CREATE TABLE IF NOT EXISTS bio (",
        "    username varchar(255),",
        "    biography text not null,",
        "    parsemode varchar(15) not null default \'plain\',",
        "    primary key (username));"
        ]
    return ()

getBio :: (IConnection conn) => conn -> String -> IO (Maybe (String, String))
getBio conn username = (do
    stmt <- prepare conn "SELECT username, biography, parsemode FROM bio WHERE username = ?"
    _ <- execute stmt [toSql username]
    result <- fetchAllRows stmt
    case result of
        [] -> return Nothing
        res:_ -> return $ Just (fromSql $ res!!1, fromSql $ res!!2)
    ) `catch` \e -> print (e :: SomeException) >> return mzero

setBio :: (IConnection conn) => conn -> String -> String -> IO ()
setBio conn username bio = (do
    stmt <- prepare conn "UPDATE bio SET biography = ? WHERE username = ?"
    rows <- execute stmt [toSql bio, toSql username]
    if rows == 0 then do
        stmt <- prepare conn "INSERT INTO bio (username, biography) VALUES (?, ?)"
        _ <- execute stmt [toSql username, toSql bio]
        commit conn
    else commit conn
    ) `catch` \e -> print (e :: SomeException)

setParseMode :: (IConnection conn) => conn -> String -> String -> IO ()
setParseMode conn username parsemode = (do
    stmt <- prepare conn "UPDATE bio SET parsemode = ? WHERE username = ?"
    _ <- execute stmt [toSql parsemode, toSql username]
    commit conn
    ) `catch` \e -> print (e :: SomeException)

getUserState :: (IConnection conn) => conn -> Int -> IO String
getUserState conn uid = (do
    stmt <- prepare conn "SELECT state FROM state WHERE uid = ?"
    _ <- execute stmt [toSql uid]
    result <- fetchAllRows stmt
    case result of
        [] -> return ""
        [res]:_ -> return . fromSql $ res
    ) `catch` \e -> print (e :: SomeException) >> return mzero

setUserState :: (IConnection conn) => conn -> Int -> String -> IO ()
setUserState conn uid state = (do
    stmt <- prepare conn "UPDATE state SET state = ? WHERE uid = ?"
    rows <- execute stmt [toSql state, toSql uid]
    if rows == 0 then do
        stmt <- prepare conn "INSERT INTO state (uid, state) VALUES (?, ?)"
        _ <- execute stmt [toSql uid, toSql state]
        commit conn
    else commit conn
    ) `catch` \e -> print (e :: SomeException)
