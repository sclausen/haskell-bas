{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Storage (
    Storage (..)
  , newStorage
) where

import           Control.Concurrent.MVar
import           Data.Storage.Purchase
import           Data.Storage.Stock
import           Data.Storage.User
import           Database.SQLite.Simple

data Storage = Storage
  { _conn           :: MVar Connection
  , _addPurchase    :: UserId -> StockId -> IO ()
  , _currentUser    :: MVar User
  , _decStockAmount :: StockId -> IO (Either String ())
  , _fetchPurchases :: Int -> Int -> IO [Purchase]
  , _fetchStock     :: StockId -> IO (Maybe Stock)
  , _fetchStocks    :: IO [Stock]
  , _fetchUser      :: String -> IO (Maybe User)
  , _incUserDebts   :: UserId -> Float -> IO ()
  }

newStorage :: IO Storage
newStorage = do
  conn <- open "bas.db"
  initialize conn
  mVarConn <- newMVar conn
  currentUser <- newEmptyMVar

  pure Storage {
    _conn = mVarConn
  , _addPurchase = addPurchase mVarConn
  , _currentUser = currentUser
  , _decStockAmount = decStockAmount mVarConn
  , _fetchPurchases = fetchPurchases mVarConn currentUser
  , _fetchStock = fetchStock mVarConn
  , _fetchStocks = fetchStocks mVarConn
  , _fetchUser = fetchUser mVarConn
  , _incUserDebts = incUserDebts mVarConn
  }

initialize :: Connection -> IO ()
initialize conn = do
   createUserTable
   createStockTable
   createPurchaseTable
    where
      createUserTable = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, publicKey  TEXT NOT NULL, debts REAL NOT NULL)"
      createStockTable = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, label TEXT NOT NULL UNIQUE, price REAL NOT NULL, amount INTEGER NOT NULL)"
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES  stock (id) ON DELETE CASCADE)"
