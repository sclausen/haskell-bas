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
fetchStocks mVarConn = withMVar mVarConn $ query_ `flip` "SELECT id, label, price, amount FROM stock ORDER BY label ASC WHERE amount > 0" :: IO [Stock]

decStockAmount :: MVar Connection -> Int -> Int -> IO (Either String ())
decStockAmount mVarConn stockId subtrahend = withMVar mVarConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - ? WHERE id = ? AND amount > 0" (subtrahend, stockId)
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "Jemand anderes war leider schneller :'("
    else pure $ Right ()




