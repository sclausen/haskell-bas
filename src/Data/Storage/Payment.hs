{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Payment (
    Payment (..)
  , libAddPayment
  , libFetchPayments
) where

import           Control.Concurrent.MVar
import           Data.Storage.User
import           Data.Time.Clock
import           Database.SQLite.Simple

type PaymentId = Int

data Payment = Payment
  { _paymentId     :: !PaymentId
  , _paymentUserId :: !UserId
  , _paymentPaidAt :: !UTCTime
  , _paymentAmount :: !Int
  } deriving (Show)

instance FromRow Payment where
  fromRow = Payment <$> field <*> field <*> field <*> field

libAddPayment :: MVar Connection -> UserId -> Int -> IO ()
libAddPayment mConn userId amount = withMVar mConn $ \conn -> execute conn "INSERT INTO payment (userId, amount) VALUES (?, ?)" (userId, amount)

libFetchPayments :: MVar Connection -> UserId -> Int -> Int -> IO [Payment]
libFetchPayments mConn userId offset limit = withMVar mConn $ \conn -> query conn "SELECT id, userId, paidAt, amount FROM payment WHERE userId = ? ORDER BY paidAt DESC LIMIT ?, ?" (userId, offset, limit)
