{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Storage (
    Storage (..)
  , newStorage
  , buy
) where

import           Control.Concurrent.MVar
import           Data.Storage.Purchase
import           Data.Storage.Stock
import           Data.Storage.User
import           Database.SQLite.Simple

data Storage = Storage
  { _conn           :: MVar Connection
  , _addPurchase    :: UserId -> StockId -> IO ()
  , _fetchUser      :: String -> IO (Maybe User)
  , _incUserDebts   :: UserId -> Float -> IO ()
  , _fetchStock     :: StockId -> IO (Maybe Stock)
  , _fetchStocks    :: IO [Stock]
  , _decStockAmount :: StockId -> IO (Either String ())
  }

newStorage :: IO Storage
newStorage = do
  conn <- open "bas.db"
  initialize conn
  mVarConn <- newMVar conn
  pure Storage {
    _conn = mVarConn
  , _addPurchase = addPurchase mVarConn
  , _fetchUser = fetchUser mVarConn
  , _incUserDebts = incUserDebts mVarConn
  , _fetchStock = fetchStock mVarConn
  , _fetchStocks = fetchStocks mVarConn
  , _decStockAmount = decStockAmount mVarConn
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

buy :: Storage -> UserId -> StockId -> IO (Either String ())
buy storage userId stockId =
  _decStockAmount storage stockId >>= \case
    Left _ -> pure $ Left ("Something weird happened, Ladies and Gentlemen!" :: String)
    Right _ ->
      _fetchStock storage stockId >>= \case
        Just stock -> do
          _incUserDebts storage userId (_price stock)
          _addPurchase storage userId stockId
          pure (Right ())
        Nothing -> pure $ Left ("Something weird happened, Ladies and Gentlemen!" :: String)
