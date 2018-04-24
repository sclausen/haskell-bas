{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , StockId
  , libAddStock
  , libUpdateStock
  , libFetchStock
  , libFetchStocks
  , libDecAndFetchStock
) where

import           Control.Concurrent.MVar
import qualified Data.Text               as T
import           Database.SQLite.Simple

type StockId = Int

data Stock = Stock
  { _stockId     :: !StockId
  , _ownerId     :: !Int
  , _stockLabel  :: T.Text
  , _stockPrice  :: !Int
  , _stockAmount :: !Int
  } deriving (Show)

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field <*> field <*> field

instance ToRow Stock where
  toRow (Stock _stockId _ownerId _stockLabel _stockPrice _stockAmount) = toRow (_stockId, _ownerId, _stockLabel, _stockPrice, _stockAmount)

libAddStock :: MVar Connection -> Stock -> IO ()
libAddStock mConn stock = withMVar mConn $ \conn -> execute conn "INSERT INTO stock (label, ownerId, price, amount) VALUES (?, ?, ?, ?)" (_stockLabel stock, _ownerId stock, _stockPrice stock, _stockAmount stock)

libFetchStocks :: MVar Connection -> IO [Stock]
libFetchStocks mConn = withMVar mConn $ query_ `flip` "SELECT id, ownerId, label, price, amount FROM stock WHERE amount > 0 ORDER BY id ASC"

libFetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
libFetchStock mConn stockId = withMVar mConn $ \conn -> query conn "SELECT id, ownerId, label, price, amount FROM stock WHERE id = ?" [stockId] >>= \case
    [!stock] -> pure $ Just stock
    _        -> pure Nothing

libUpdateStock :: MVar Connection -> StockId -> Int -> IO Bool
libUpdateStock mConn stockId newAmount = withMVar mConn $ \conn -> do
  execute conn "UPDATE stock SET amount = ? WHERE id = ?" (newAmount, stockId)
  (/= 0) <$> changes conn

libDecStockAmount :: MVar Connection -> StockId -> IO (Either String ())
libDecStockAmount mConn stockId = withMVar mConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - 1 WHERE id = ? AND amount > 0" [stockId]
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "The stock couldn't be decreased"
    else pure $ Right ()

libDecAndFetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
libDecAndFetchStock mConn stockId = libDecStockAmount mConn stockId >>= \case
  Left _ -> pure Nothing
  Right () -> libFetchStock mConn stockId >>= \case
    Just !stock -> pure $ Just stock
    Nothing     -> pure Nothing


