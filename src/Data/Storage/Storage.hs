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
initialize conn = do
   createUserTable
   createStockTable
   createPurchaseTable
    where
      createUserTable = execute_ conn $ [r|
        CREATE TABLE IF NOT EXISTS user
        (
          id         INTEGER PRIMARY KEY,
          username   TEXT UNIQUE,
          public_key TEXT,
          debts      REAL
        )
        |]
      createStockTable = execute_ conn $ [r|
        CREATE TABLE IF NOT EXISTS stock
        (
          id         INTEGER PRIMARY KEY,
          label      TEXT UNIQUE,
          price      REAL
        )
        |]
      createPurchaseTable = execute_ conn $ [r|
        CREATE TABLE IF NOT EXISTS purchase
        (
          id         INTEGER PRIMARY KEY,
          userId     INTEGER,
          stockId    INTEGER,
          boughtAt   TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE,
          FOREIGN KEY(stockId) REFERENCES  stock (id) ON DELETE CASCADE
        )
        |]
