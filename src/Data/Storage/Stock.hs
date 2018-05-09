{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , StockId
  , addStock
  , updateStock
  , fetchStock
  , fetchStocks
  , decAndFetchStock
) where

import           Control.Concurrent.MVar
--import           Data.Either.Combinators
import           Database.SQLite.Simple

type StockId = Int

data Stock = Stock
  { _stockId     :: !StockId
  , _ownerId     :: !Int
  , _stockLabel  :: String
  , _stockPrice  :: !Int
  , _stockAmount :: !Int
  } deriving (Show)

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field <*> field <*> field

instance ToRow Stock where
  toRow (Stock _stockId _ownerId _stockLabel _stockPrice _stockAmount) = toRow (_stockId, _ownerId, _stockLabel, _stockPrice, _stockAmount)

addStock :: MVar Connection -> Stock -> IO ()
addStock mConn stock = withMVar mConn $ \conn -> execute conn "INSERT INTO stock (label, ownerId, price, amount) VALUES (?, ?, ?, ?)" (_stockLabel stock, _ownerId stock, _stockPrice stock, _stockAmount stock)

fetchStocks :: MVar Connection -> IO [Stock]
fetchStocks mConn = withMVar mConn $ query_ `flip` "SELECT id, ownerId, label, price, amount FROM stock WHERE amount > 0 ORDER BY id ASC"

fetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
fetchStock mConn stockId = withMVar mConn $ \conn -> query conn "SELECT id, ownerId, label, price, amount FROM stock WHERE id = ?" [stockId] >>= \case
    [!stock] -> pure $ Just stock
    _        -> pure Nothing

updateStock :: MVar Connection -> StockId -> Int -> IO Bool
updateStock mConn stockId newAmount = withMVar mConn $ \conn -> do
  execute conn "UPDATE stock SET amount = ? WHERE id = ?" (newAmount, stockId)
  (/= 0) <$> changes conn

decStockAmount :: MVar Connection -> StockId -> IO (Either String ())
decStockAmount mConn stockId = withMVar mConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - 1 WHERE id = ? AND amount > 0" [stockId]
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "The stock couldn't be decreased"
    else pure $ Right ()

decAndFetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
decAndFetchStock mConn stockId = decStockAmount mConn stockId >>= \case
  Left _ -> pure Nothing
  Right () -> fetchStock mConn stockId >>= \case
    Just !stock -> pure $ Just stock
    Nothing     -> pure Nothing


