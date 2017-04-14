{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module DB where

import Data.TCache
import Data.TCache.Defs
import Data.TCache.IndexQuery as IQ
import Data.TCache.DefaultPersistence

import GHC.Generics (Generic)

import qualified Data.Serialize as S

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Data.Typeable (Typeable)

import Control.Exception
import Control.Monad (mzero)

data User = User {
    uid :: Int,
    username :: String,
    biography :: String,
    parsemode :: String,
    state :: String
} deriving (Show, Read, Typeable, Generic)
defaultUser = User{uid = 0, username = "", biography = "", parsemode = "plain", state = ""}

uidKey uid = "User/" ++ show uid

instance S.Serialize User

instance Indexable User where
    key User{uid = uid, username = username}
        | uid /= 0 = uidKey uid
        | otherwise = "User/name/" ++ username 
instance Serializable User where
    serialize = S.encodeLazy
    deserialize = either error id . S.decodeLazy

initDB = do
    IQ.index uid
    IQ.index username

getBio :: String -> IO (Maybe (String, String))
getBio uname = atomically $ do
    us <- username .==. uname
    case us of
        u:_ -> do
            user <- readDBRef u `onNothing` error "unexpected Nothing"
            return $ Just (biography user, parsemode user)
        [] -> return Nothing

setBio uid uname bio = do
    let u'' = getDBRef $ "User/name/" ++ uname :: DBRef User
    mu <- readDBRef u''
    case mu of
        Nothing -> return ()
        Just _ -> delDBRef u''
    let u' = getDBRef $ uidKey uid
    olduser <- readDBRef u' `onNothing` return defaultUser{uid = uid, username = uname, biography = bio}
    writeDBRef u' $ olduser {username = uname, biography = bio}

setParseMode uid parsemode = do
    let u' = getDBRef $ uidKey uid
    userm <- readDBRef u' `onNothing` return defaultUser{uid = uid}
    writeDBRef u' $ userm {parsemode = parsemode}

getUserState uid = atomically $ do
    userm <- readDBRef . getDBRef $ uidKey uid
    return $ maybe "" state userm

setUserState uid s = do
    let u' = getDBRef $ uidKey uid
    userm <- readDBRef u' `onNothing` return defaultUser{uid = uid}
    writeDBRef u' $ userm {state = s}

importUserBio :: [(String, String, String)] -> STM ()
importUserBio = mapM_ (writeUser . convUser) where
    convUser (u, b, p) = User{uid = 0, username = u, biography = b, parsemode = p, state = ""}
    writeUser u = writeDBRef (getDBRef $ key u) u