{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.UserStorage (
      User
    , newUserStorage
    , UserStorage
  )
  where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

data UserStorage = UserStorage { _conn      :: MVar Connection
                               , _fetchUser :: String -> IO (Maybe User)
                               , _incDepts  :: String -> Float -> IO ()
                               , _decDepts  :: String -> Float -> IO ()
}

newUserStorage :: MVar Connection -> IO UserStorage
newUserStorage mVarConn = do
  initialize mVarConn
  pure $ UserStorage {
    _conn = mVarConn
  , _fetchUser = fetchUser mVarConn
  , _incDepts = incDepts mVarConn
  , _decDepts = decDepts mVarConn
  }

data User = User { _id        :: !Int
                 , _username  :: String
                 , _publicKey :: String
                 , _depts     :: !Float
} deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

initialize :: MVar Connection -> IO ()
initialize mVarConn = withMVar mVarConn $ execute_ `flip` "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT unique, public_key TEXT, depts REAL)"

fetchUser :: MVar Connection -> String -> IO (Maybe User)
fetchUser mVarConn username = withMVar mVarConn $ fetchUser' `flip` username
  where
    fetchUser' conn username = do
      users <- query conn "SELECT id, username, public_key, depts from user WHERE username = ?" [username] :: IO [User]
      case users of
        []       -> pure Nothing
        [user]   -> pure $ Just user
        (user:_) -> pure Nothing -- Should never happen

incDepts :: MVar Connection -> String -> Float -> IO ()
incDepts mVarConn username factor = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET depts = depts + ? WHERE username = ?" (factor, username)

decDepts :: MVar Connection -> String -> Float -> IO ()
decDepts mVarConn username factor = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET depts = depts - ? WHERE username = ?" (factor, username)

