{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Storage.Storage
import           Data.Storage.User
import           Database.SQLite.Simple
import           System.Directory
import           System.Environment
import           System.IO

main :: IO ()
main = do
  (dbFile, _)      <- openTempFile "/tmp" "bas.db"

  setEnv "DB_FILE" dbFile
  setEnv "BAS_USER" "foo"

  initialize =<< open =<< getEnv "DB_FILE"
  storage <- newStorage
  let mConn = _conn storage

  let stockId = 1 :: Int
  let userId = 1 :: UserId
  void $ _addPurchase storage stockId
  void $ _decAndFetchStock storage stockId
  void $ _fetchPurchases storage 0 10
  void $ _fetchStock storage stockId
  void $ _fetchStocks storage
  void $ _fetchUserByName storage "foo"
  void $ _incUserDebts storage userId 100
  void $ _fetchPayments storage userId 0 10
  void $ _addPayment storage userId 100

  --removeFile dbFile

initialize :: Connection -> IO ()
initialize conn = do
    createUserTable
    createUser
    createStockTable
    createStock
    createPurchaseTable
    where
      createUserTable = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts INTEGER NOT NULL DEFAULT 0, isAdmin INTEGER NOT NULL)"
      createUser = execute conn "INSERT INTO user (id, username, debts, isAdmin) VALUES (?, ?, ?, ?)" (1 :: Int, "foo" :: String, 0 :: Int, 1 :: Int)
      createStockTable = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, ownerId INTEGER NOT NULL , label TEXT NOT NULL, price INTEGER NOT NULL, amount INTEGER NOT NULL, FOREIGN KEY(ownerId) REFERENCES user (id) ON DELETE CASCADE)"
      createStock = execute conn "INSERT INTO stock (id, ownerId,  label, price, amount) VALUES (?,?,?,?,?)" (1 :: Int, 1 :: Int, "Test Beverage" :: String, 123 :: Int, 100 :: Int)
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES  stock (id) ON DELETE CASCADE)"
