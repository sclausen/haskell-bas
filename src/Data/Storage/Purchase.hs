{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Purchase (
    Purchase (..)
  , addPurchase
  , fetchPurchases
  , prettyPrintPurchase
  , prettyPrintPurchase'
) where

import           Control.Concurrent.MVar
import           Data.Storage.Stock
import           Data.Storage.User
import           Data.Time.Clock
import           Database.SQLite.Simple
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

fetchPurchases :: MVar Connection -> MVar User -> Int -> Int -> IO [Purchase]
fetchPurchases mConn mUser offset limit = do
  user <- readMVar mUser
  withMVar mConn $ \conn -> query conn "SELECT purchase.id, userId, stockId, boughtAt, stock.price, stock.label FROM purchase LEFT JOIN stock ON stock.id = purchase.stockId WHERE userId = ? ORDER BY boughtAt DESC LIMIT ?, ?" (_userId user, offset, limit)

prettyPrintPurchase :: [Purchase] -> IO ()
prettyPrintPurchase ps = do
  print $ PP.black $ PP.ondullwhite $ PP.fill 15 (PP.text "Product") PP.<+> PP.fill 6 (PP.text "Price") PP.<+> PP.fill 23 (PP.text "Date")
  print $ PP.vcat $ fmap (\p ->
    PP.fill 15 (PP.text $ _stockLabel p) PP.<+>
    PP.fill 6 (PP.text $ printf "%.2f€" $ _sPrice p) PP.<+>
    PP.fill 23 (PP.text $ show $ _boughtAt p)
    ) ps

prettyPrintPurchase' :: [Purchase] -> String
prettyPrintPurchase' ps = show $ PP.vcat $ headerRow:purchaseRows
  where
    headerRow = PP.black $ PP.ondullwhite $ PP.fill 15 (PP.text "Product") PP.<+> PP.fill 6 (PP.text "Price") PP.<+> PP.fill 23 (PP.text "Date")
    purchaseRows = fmap (\p ->
      PP.fill 15 (PP.text $ _stockLabel p) PP.<+>
      PP.fill 6 (PP.text $ printf "%.2f€" $ _sPrice p) PP.<+>
      PP.fill 23 (PP.text $ show $ _boughtAt p)
      ) ps
