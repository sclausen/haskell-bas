{-# LANGUAGE LambdaCase        #-}
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
import           System.Environment
import           System.Exit

data Storage = Storage
  { _conn             :: MVar Connection
  , _currentUser      :: MVar User
  , _addPurchase      :: StockId -> IO ()
  , _decAndFetchStock :: StockId -> IO (Maybe Stock)
  , _fetchPurchases   :: Int -> Int -> IO [Purchase]
  , _fetchStock       :: StockId -> IO (Maybe Stock)
  , _fetchStocks      :: IO [Stock]
  , _fetchUser        :: String -> IO (Maybe User)
  , _incUserDebts     :: Int -> IO ()
  }

newStorage :: IO Storage
newStorage = do
  conn <- open =<< getEnv "DB_FILE"
  initialize conn
  mVarConn <- newMVar conn
  username <- getEnv "BAS_USER"
  fetchUser mVarConn username >>= \case
    Just user -> do
      currentUser <- newMVar user
      pure Storage
        { _conn = mVarConn
        , _addPurchase = addPurchase mVarConn currentUser
        , _currentUser = currentUser
        , _decAndFetchStock = decAndFetchStock mVarConn
        , _fetchPurchases = fetchPurchases mVarConn currentUser
        , _fetchStock = fetchStock mVarConn
        , _fetchStocks = fetchStocks mVarConn
        , _fetchUser = fetchUser mVarConn
        , _incUserDebts = incUserDebts mVarConn currentUser
        }
    _ -> exitFailure

initialize :: Connection -> IO ()
initialize conn = do
   createUserTable
   createStockTable
   createPurchaseTable
    where
      createUserTable = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts INTEGER NOT NULL)"
      createStockTable = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, label TEXT NOT NULL UNIQUE, price INTEGER NOT NULL, amount INTEGER NOT NULL)"
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES  stock (id) ON DELETE CASCADE)"
