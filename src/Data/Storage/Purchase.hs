{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Purchase (
    Purchase (..)
  , addPurchase
) where

import           Control.Concurrent.MVar
import           Data.Storage.Stock
import           Data.Storage.User
import           Database.SQLite.Simple

type PurchaseId = Int

data Purchase = Purchase
  { _purchaseId :: !PurchaseId
  , _userId     :: !UserId
  , _stockId    :: !StockId
  , _boughtAt   :: !Int
  }

instance FromRow Purchase where
  fromRow = Purchase <$> field <*> field <*> field <*> field

addPurchase :: MVar Connection -> UserId -> StockId -> IO ()
addPurchase mConn userId stockId = withMVar mConn $ \conn -> execute conn "INSERT INTO purchase (userId, stockId) VALUES (?, ?)" (userId, stockId)
