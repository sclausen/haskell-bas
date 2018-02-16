{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Data.Storage.Storage
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
  void $ _addPurchase storage stockId
  void $ _decAndFetchStock storage stockId
  void $ _fetchPurchases storage 0 10
  void $ _fetchStock storage stockId
  void $ _fetchStocks storage
  void $ _fetchUser storage "foo"
  void $ _incUserDebts storage 1.0

  removeFile dbFile

initialize :: Connection -> IO ()
initialize conn = do
    createUserTable
    createUser
    createStockTable
    createStock
    createPurchaseTable
    where
      createUserTable = execute_ conn "CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts REAL NOT NULL)"
      createUser = execute conn "INSERT INTO user (id, username, debts) VALUES (?, ?, ?)" (1 :: Int, "foo" :: String, 0 :: Float)
      createStockTable = execute_ conn "CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, label TEXT NOT NULL UNIQUE, price REAL NOT NULL, amount INTEGER NOT NULL)"
      createStock = execute conn "INSERT INTO stock (id, label, price, amount) VALUES (?,?,?,?)" (1 :: Int, "Test Beverage" :: String, 1.23 :: Float, 100 :: Int)
      createPurchaseTable = execute_ conn "CREATE TABLE IF NOT EXISTS purchase (id INTEGER PRIMARY KEY, userId INTEGER NOT NULL, stockId INTEGER NOT NULL, boughtAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP, FOREIGN KEY(userId) REFERENCES user (id) ON DELETE CASCADE, FOREIGN KEY(stockId) REFERENCES  stock (id) ON DELETE CASCADE)"
