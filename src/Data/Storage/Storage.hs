{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Data.Storage.Storage (
    Storage (..)
  , newStorage
) where

import           Control.Concurrent.MVar
import           Data.Storage.User
import           Database.SQLite.Simple
import           Text.RawString.QQ

data Storage = Storage
  { _conn      :: MVar Connection
  , _fetchUser :: String
               -> IO (Maybe User)
  , _incDebts  :: String
               -> Float
               -> IO ()
  }

newStorage :: IO Storage
newStorage = do
  conn <- open "bas.db"
  initialize conn
  mVarConn <- newMVar conn
  pure Storage {
    _conn = mVarConn
  , _fetchUser = fetchUser mVarConn
  , _incDebts = incDebts mVarConn
  }

initialize :: Connection
           -> IO ()
initialize conn = execute_ conn $ [r|
CREATE TABLE IF NOT EXISTS user
(
   id         INTEGER PRIMARY KEY,
   username   TEXT UNIQUE,
   public_key TEXT,
   debts      REAL
)
|]
