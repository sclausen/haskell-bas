{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Purchase (
    Purchase (..)
  , addPurchase
  , fetchPurchases
) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Storage.Stock
import           Data.Storage.User
import           Data.Time.Clock
import           Database.SQLite.Simple
import           System.IO                    (hReady, hSetEcho, stdin)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf

type PurchaseId = Int

data Purchase = Purchase
  { _purchaseId :: !PurchaseId
  , _pUserId    :: !UserId
  , _pStockId   :: !StockId
  , _boughtAt   :: !UTCTime
  , _sPrice     :: !Float
  , _stockLabel :: String
  } deriving (Show)

instance FromRow Purchase where
  fromRow = Purchase <$> field <*> field <*> field <*> field <*> field <*> field

addPurchase :: MVar Connection -> UserId -> StockId -> IO ()
addPurchase mConn userId stockId = withMVar mConn $ \conn -> execute conn "INSERT INTO purchase (userId, stockId) VALUES (?, ?)" (userId, stockId)

fetchPurchases :: MVar Connection -> MVar User -> Int -> Int -> IO ()
fetchPurchases mConn mUser offset limit = do
  user      <- readMVar mUser
  purchases <- withMVar mConn $ \conn -> query conn "SELECT purchase.id, userId, stockId, boughtAt, stock.price, stock.label FROM purchase LEFT JOIN stock ON stock.id = purchase.stockId WHERE userId = ? ORDER BY boughtAt DESC LIMIT ?, ?" (_userId user, offset, limit)
  case purchases of
    [] -> pure ()
    _  -> when (offset == 0) printHeader >> printPurchases purchases
 where
  printPurchases ps = do
    prettyPrintPurchases ps
    getCharHidden (fetchPurchase mConn mUser (offset + 1))

fetchPurchase :: MVar Connection -> MVar User -> Int -> IO ()
fetchPurchase mConn mUser offset = do
  user      <- readMVar mUser
  purchases <- withMVar mConn $ \conn -> query conn "SELECT purchase.id, userId, stockId, boughtAt, stock.price, stock.label FROM purchase LEFT JOIN stock ON stock.id = purchase.stockId WHERE userId = ? ORDER BY boughtAt DESC LIMIT ?, 1" (_userId user, offset)
  case purchases of
    [purchase] -> prettyPrintPurchase purchase >> waitForNext
    _          -> pure ()
  where waitForNext = getCharHidden (fetchPurchase mConn mUser (offset + 1))


printHeader :: IO ()
printHeader =
  print
    $      PP.bold
    $      PP.black
    $      PP.ondullwhite
    $      PP.fill 15 (PP.text "Product")
    PP.<+> PP.fill 6  (PP.text "Price")
    PP.<+> PP.fill 23 (PP.text "Date")

prettyPrintPurchases :: [Purchase] -> IO ()
prettyPrintPurchases ps = print $ PP.vcat $ fmap
  ( \p ->
    PP.fill 15 (PP.text $ _stockLabel p) PP.<+>
    PP.fill 6  (PP.text $ printf "%.2f€" $ _sPrice p) PP.<+>
    PP.fill 23 (PP.text $ show $ _boughtAt p)
  )
  ps

prettyPrintPurchase :: Purchase -> IO ()
prettyPrintPurchase p =
  print
    $      PP.fill 15 (PP.text $ _stockLabel p)
    PP.<+> PP.fill 6  (PP.text $ printf "%.2f€" $ _sPrice p)
    PP.<+> PP.fill 23 (PP.text $ show $ _boughtAt p)

getCharHidden :: IO () -> IO ()
getCharHidden cb = do
  hSetEcho stdin False
  key <- getKey
  case key of
    "\n" -> cb
    _    -> return ()

getKey :: IO String
getKey = reverse <$> getKey' ""
 where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char : chars)
