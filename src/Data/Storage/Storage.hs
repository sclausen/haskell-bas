{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Storage (
    Storage (..)
  , newStorage
) where

import           Control.Concurrent.MVar
import           Data.Storage.Payment
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
  , _fetchUserByName  :: String -> IO (Maybe User)
  , _fetchUser        :: UserId -> IO (Maybe User)
  , _fetchUsers       :: IO [User]
  , _incUserDebts     :: UserId -> Int -> IO ()
  , _addPayment       :: UserId -> Int -> IO ()
  , _fetchPayments    :: UserId -> Int -> Int -> IO [Payment]
  }

newStorage :: IO Storage
newStorage = do
  conn <- open =<< getEnv "DB_FILE"
  initialize conn
  mVarConn <- newMVar conn
  username <- getEnv "BAS_USER"
  fetchUserByName mVarConn username >>= \case
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
        , _fetchUsers = fetchUsers mVarConn
        , _fetchUserByName = fetchUserByName mVarConn
        , _fetchUser = fetchUser mVarConn
        , _incUserDebts = incUserDebts mVarConn
        , _addPayment = addPayment mVarConn
        , _fetchPayments = fetchPayments mVarConn
        }
    _ -> exitFailure

initialize :: Connection -> IO ()
initialize conn = do
   createUserTable
   createStockTable
   createPurchaseTable
   createPaymentTable
    where
      createUserTable     = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts INTEGER NOT NULL, isAdmin INTEGER NOT NULL)"
      createStockTable    = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, label TEXT NOT NULL UNIQUE, price INTEGER NOT NULL, amount INTEGER NOT NULL)"
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES stock (id) ON DELETE CASCADE)"
      createPaymentTable  = execute_ conn "CREATE TABLE IF NOT EXISTS payment (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, paidAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, amount INTEGER NOT NULL, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE)"
