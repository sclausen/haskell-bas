{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , fetchStocks
  , decStockAmount
) where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

data Stock = Stock
  { _id     :: !Int
  , _label  :: String
  , _price  :: !Float
  , _amount :: !Int
  } deriving (Show)

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field <*> field

fetchStocks :: MVar Connection -> IO [Stock]
fetchStocks mVarConn = withMVar mVarConn $ query_ `flip` "SELECT id, label, price, amount FROM stock ORDER BY label ASC" :: IO [Stock]

decStockAmount :: MVar Connection -> Int -> Int -> IO ()
decStockAmount mVarConn stockId subtrahend = withMVar mVarConn $ \conn -> execute conn "UPDATE stock SET amount = amount - ? WHERE id = ?" (subtrahend, stockId)
