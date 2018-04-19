{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Storage (
    Storage (..)
  , newStorage
) where
-- Testst
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
  , _addStock         :: Stock -> IO ()
  , _updateStock      :: StockId -> Int -> IO Bool
  , _decAndFetchStock :: StockId -> IO (Maybe Stock)
  , _fetchStock       :: StockId -> IO (Maybe Stock)
  , _fetchStocks      :: IO [Stock]
  , _currentUser      :: MVar User
  , _addUser          :: User -> IO Bool
  , _updateUser       :: UserId -> Bool -> IO Bool
  , _deleteUser       :: Username -> IO Bool
  , _fetchUserByName  :: String -> IO (Maybe User)
  , _fetchUser        :: UserId -> IO (Maybe User)
  , _fetchUsers       :: IO [User]
  , _incUserDebts     :: UserId -> Int -> IO ()
  , _addPayment       :: UserId -> Int -> IO ()
  , _fetchPayments    :: UserId -> Int -> Int -> IO [Payment]
  , _addPurchase      :: StockId -> IO ()
  , _fetchPurchases   :: Int -> Int -> IO [Purchase]
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
        , _addStock = addStock mVarConn
        , _updateStock = updateStock mVarConn
        , _decAndFetchStock = decAndFetchStock mVarConn
        , _fetchStock = fetchStock mVarConn
        , _fetchStocks = fetchStocks mVarConn
        , _currentUser = currentUser
        , _addUser = addUser mVarConn
        , _updateUser = updateUser mVarConn
        , _deleteUser = deleteUser mVarConn
        , _fetchUsers = fetchUsers mVarConn
        , _fetchUserByName = fetchUserByName mVarConn
        , _fetchUser = fetchUser mVarConn
        , _incUserDebts = incUserDebts mVarConn
        , _addPayment = addPayment mVarConn
        , _fetchPayments = fetchPayments mVarConn
        , _addPurchase = addPurchase mVarConn currentUser
        , _fetchPurchases = fetchPurchases mVarConn currentUser
        }
    _ -> exitFailure

initialize :: Connection -> IO ()
initialize conn = do
   createUserTable
   createStockTable
   createPurchaseTable
   createPaymentTable
    where
      createUserTable     = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts INTEGER NOT NULL DEFAULT 0, isAdmin INTEGER NOT NULL)"
      createStockTable    = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, ownerId INTEGER NOT NULL , label TEXT NOT NULL UNIQUE, price INTEGER NOT NULL, amount INTEGER NOT NULL, FOREIGN KEY(ownerId) REFERENCES user (id) ON DELETE CASCADE)"
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES stock (id) ON DELETE CASCADE)"
      createPaymentTable  = execute_ conn "CREATE TABLE IF NOT EXISTS payment (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, paidAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, amount INTEGER NOT NULL, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE)"
