{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
import           Lens.Micro.TH
import           System.Environment
import           System.Exit

instance Show Storage where
  show a = "unrenderable"

data Storage = Storage
  { _conn                      :: MVar Connection
  , _addStock                  :: Stock -> IO ()
  , _updateStock               :: StockId -> Int -> IO Bool
  , _decAndFetchStock          :: StockId -> IO (Maybe Stock)
  , _fetchStock                :: StockId -> IO (Maybe Stock)
  , _fetchStocks               :: IO [Stock]
  , _currentUser               :: MVar User
  , _addUser                   :: User -> IO Bool
  , _updateUser                :: UserId -> Bool -> IO Bool
  , _deleteUser                :: Username -> IO Bool
  , _fetchUserByName           :: String -> IO (Maybe User)
  , _fetchUser                 :: UserId -> IO (Maybe User)
  , _fetchUsers                :: IO [User]
  , _incUserDebts              :: UserId -> Int -> IO ()
  , _addPayment                :: UserId -> Int -> IO ()
  , _fetchPayments             :: UserId -> Int -> Int -> IO [Payment]
  , _addPurchase               :: StockId -> IO ()
  , _fetchOutstandingPurchases :: IO [OutstandingPurchase]
  }

makeLenses ''Storage

newStorage :: IO Storage
newStorage = do
  -- conn <- open =<< getEnv "DB_FILE"
  conn <- open "./bas.db"
  initialize conn
  mVarConn <- newMVar conn
  let username = "sclausen"
  -- username <- getEnv "BAS_USER"
  libFetchUserByName mVarConn username >>= \case
    Just user -> do
      libCurrentUser <- newMVar user
      pure Storage
        { _conn = mVarConn
        , _addStock = libAddStock mVarConn
        , _updateStock = libUpdateStock mVarConn
        , _decAndFetchStock = libDecAndFetchStock mVarConn
        , _fetchStock = libFetchStock mVarConn
        , _fetchStocks = libFetchStocks mVarConn
        , _currentUser = libCurrentUser
        , _addUser = libAddUser mVarConn
        , _updateUser = libUpdateUser mVarConn
        , _deleteUser = libDeleteUser mVarConn
        , _fetchUsers = libFetchUsers mVarConn
        , _fetchUserByName = libFetchUserByName mVarConn
        , _fetchUser = libFetchUser mVarConn
        , _incUserDebts = libIncUserDebts mVarConn
        , _addPayment = libAddPayment mVarConn
        , _fetchPayments = libFetchPayments mVarConn
        , _addPurchase = libAddPurchase mVarConn libCurrentUser
        , _fetchOutstandingPurchases = libFetchOutstandingPurchases mVarConn libCurrentUser
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
