{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Purchase (
    Purchase (..)
  , OutstandingPurchase (..)
  , libAddPurchase
  , libFetchOutstandingPurchases
) where

import           Control.Concurrent.MVar
import           Data.Storage.Stock
import           Data.Storage.User
import           Data.Time.Clock
import           Database.SQLite.Simple

type PurchaseId = Int

data Purchase = Purchase
  { _purchaseId         :: !PurchaseId
  , _purchaseUserId     :: !UserId
  , _purchaseStockId    :: !StockId
  , _purchaseBoughtAt   :: !UTCTime
  , _purchasePrice      :: !Int
  , _purchaseStockLabel :: String
  , _paidAt             :: !UTCTime
  } deriving (Show)

data OutstandingPurchase = OutstandingPurchase
  { _stockId    :: !StockId
  , _debts      :: Int
  , _creditorId :: !UserId
  }

instance FromRow Purchase where
  fromRow = Purchase <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow OutstandingPurchase where
  fromRow = OutstandingPurchase <$> field <*> field <*> field

libAddPurchase :: MVar Connection -> MVar User -> StockId -> IO ()
libAddPurchase mConn mUser stockId = do
  user <- readMVar mUser
  withMVar mConn $ \conn -> execute conn "INSERT INTO purchase (userId, stockId) VALUES (?, ?)" (_userId user, stockId)

libFetchOutstandingPurchases :: MVar Connection -> MVar User -> IO [OutstandingPurchase]
libFetchOutstandingPurchases mConn mUser = do
  user <- readMVar mUser
  withMVar mConn $ \conn -> query conn "SELECT stock.id as stockId, SUM(stock.price) AS debts, user.id AS creditorId FROM purchase LEFT JOIN stock ON stock.id = purchase.stockId LEFT JOIN user ON stock.ownerId = user.id WHERE  purchase.paidAt IS NULL AND purchase.userId  = ? GROUP BY stock.id" (Only $ _userId user)
