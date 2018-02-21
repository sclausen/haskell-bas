{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Purchase (
    Purchase (..)
  , addPurchase
  , fetchPurchases
) where

import           Control.Concurrent.MVar
import           Data.Storage.Stock
import           Data.Storage.User
import           Data.Time.Clock
import           Database.SQLite.Simple


type PurchaseId = Int

data Purchase = Purchase
  { _purchaseId :: !PurchaseId
  , _pUserId    :: !UserId
  , _pStockId   :: !StockId
  , _boughtAt   :: !UTCTime
  , _sPrice     :: !Int
  , _stockLabel :: String
  } deriving (Show)

instance FromRow Purchase where
  fromRow = Purchase <$> field <*> field <*> field <*> field <*> field <*> field

addPurchase :: MVar Connection -> MVar User -> StockId -> IO ()
addPurchase mConn mUser stockId = do
  user <- readMVar mUser
  withMVar mConn $ \conn -> execute conn "INSERT INTO purchase (userId, stockId) VALUES (?, ?)" (_userId user, stockId)

fetchPurchases :: MVar Connection -> MVar User -> Int -> Int -> IO [Purchase]
fetchPurchases mConn mUser offset limit = do
  user <- readMVar mUser
  withMVar mConn $ \conn -> query conn "SELECT purchase.id, userId, stockId, boughtAt, stock.price, stock.label FROM purchase LEFT JOIN stock ON stock.id = purchase.stockId WHERE userId = ? ORDER BY boughtAt DESC LIMIT ?, ?" (_userId user, offset, limit)
