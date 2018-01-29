{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.UserStorage (
      User
    , newUserStorage
    , UserStorage
  )
  where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

data UserStorage = UserStorage { _conn  :: MVar Connection
                               , _fetch :: String -> IO (Maybe User)
}

newUserStorage :: IO UserStorage
newUserStorage = do
  conn <- open "test.db"
  initialize conn
  mVarConn <- newMVar conn
  pure $ UserStorage {
    _conn = mVarConn
  , _fetch = fetch' mVarConn
  }

data User = User { _id        :: Int
                 , _username  :: String
                 , _publicKey :: String
                 , _depts     :: Float
} deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

initialize :: Connection -> IO ()
initialize conn = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT unique, public_key TEXT, depts FLOAT)"

add :: Connection -> String -> String -> IO ()
add _ "" _ = pure ()
add _ _ "" = pure ()
add conn username publicKey = execute conn "INSERT INTO user (username, public_key, depts) VALUES (?, ?, ?)" (username, publicKey, 0 :: Float)

remove :: Connection -> String -> IO ()
remove conn username = execute conn "DELETE FROM user WHERE username = ?" (Only username)

fetch :: Connection -> String -> IO (Maybe User)
fetch _ "" = pure Nothing
fetch conn username = do
  users <-  query conn "SELECT id, username, public_key, depts from user WHERE username = ?" [username] :: IO [User]
  case users of
    []       -> pure Nothing
    [user]   -> pure $ Just user
    (user:_) -> pure Nothing -- Should never happen

fetch' :: MVar Connection -> String -> IO (Maybe User)
fetch' mVarConn username = do
  conn <- takeMVar mVarConn
  mUser <- fetch conn username
  putMVar mVarConn conn
  pure mUser


showUser :: Maybe User -> IO ()
showUser mUser = case mUser of
  Nothing   -> print "Gibbet net"
  Just user -> print $ _username user

