{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , StockId
  , fetchStock
  , fetchStocks
  , decStockAmount
) where

import           Control.Concurrent.MVar
import           Data.Either.Combinators
import           Database.SQLite.Simple

type StockId = Int

data Stock = Stock
  { _stockId :: !StockId
  , _label   :: String
  , _price   :: !Float
  , _amount  :: !Int
  } deriving (Show)

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field <*> field

fetchStocks :: MVar Connection -> IO [Stock]
fetchStocks mVarConn = withMVar mVarConn $ query_ `flip` "SELECT id, label, price, amount FROM stock WHERE amount > 0 ORDER BY id ASC" :: IO [Stock]

fetchStock :: MVar Connection -> StockId -> IO (Maybe Stock)
fetchStock mVarConn stockId = do
  stocks <- withMVar mVarConn $ \conn -> query conn "SELECT id, label, price, amount FROM stock WHERE id = ?" [stockId] :: IO [Stock]
  case stocks of
    [stock] -> pure $ Just stock
    _       -> pure Nothing

decStockAmount :: MVar Connection -> StockId -> IO (Either String Stock)
decStockAmount mVarConn stockId = withMVar mVarConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - 1 WHERE id = ? AND amount > 0" [stockId]
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "By now the stock has already been depleted!"
    else  maybeToRight "fetching failed" <$> fetchStock mVarConn stockId
