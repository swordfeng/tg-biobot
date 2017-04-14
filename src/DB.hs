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

userKey uid = "User " ++ show uid

instance S.Serialize User

instance Indexable User where
    key User{uid = uid} = userKey uid
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
    let u' = getDBRef $ userKey uid
    olduser <- readDBRef u' `onNothing` return User{uid = uid, username = uname, biography = bio, parsemode = "plain", state = ""}
    writeDBRef u' $ olduser {username = uname, biography = bio}

setParseMode uid parsemode = do
    let u' = getDBRef $ userKey uid
    userm <- readDBRef u'
    maybe mzero (\u -> writeDBRef u' $ (u::User) {parsemode = parsemode}) userm

getUserState uid = atomically $ do
    userm <- readDBRef . getDBRef $ userKey uid
    return $ maybe "" state userm

setUserState uid s = do
    let u' = getDBRef $ userKey uid
    userm <- readDBRef u'
    maybe mzero (\u -> writeDBRef u' $ (u::User) {state = s}) userm
