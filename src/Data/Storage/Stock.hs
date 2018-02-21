{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , StockId
  , fetchStock
  , fetchStocks
  , decAndFetchStock
) where

import           Control.Concurrent.MVar
--import           Data.Either.Combinators
import           Database.SQLite.Simple

type StockId = Int

data Stock = Stock
  { _stockId :: !StockId
  , _label   :: String
  , _price   :: !Int
  , _amount  :: !Int
  } deriving (Show)

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field <*> field

fetchStocks :: MVar Connection -> IO [Stock]
fetchStocks mVarConn = withMVar mVarConn $ query_ `flip` "SELECT id, label, price, amount FROM stock WHERE amount > 0 ORDER BY id ASC"

fetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
fetchStock mVarConn stockId = withMVar mVarConn $ \conn -> query conn "SELECT id, label, price, amount FROM stock WHERE id = ?" [stockId] >>= \case
    [!stock] -> pure $ Just stock
    _        -> pure Nothing

decStockAmount :: MVar Connection -> StockId -> IO (Either String ())
decStockAmount mVarConn stockId = withMVar mVarConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - 1 WHERE id = ? AND amount > 0" [stockId]
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "The stock couldn't be decreased"
    else pure $ Right ()

decAndFetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
decAndFetchStock mVarConn stockId = decStockAmount mVarConn stockId >>= \case
  Left _ -> pure Nothing
  Right () -> fetchStock mVarConn stockId >>= \case
    Just !stock -> pure $ Just stock
    Nothing     -> pure Nothing


