{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.Stock (
    Stock (..)
  , StockId
  , fetchStock
  , fetchStocks
  , decStockAmount
  , prettyPrintStocks
) where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf

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
  stocks <- withMVar mVarConn $ \conn-> query conn "SELECT id, label, price, amount FROM stock WHERE id = ?" [stockId] :: IO [Stock]
  case stocks of
    [stock] -> pure $ Just stock
    []      -> pure Nothing
    (_:_)   -> pure Nothing -- should not happen, since the id is unique

decStockAmount :: MVar Connection -> StockId -> IO (Either String ())
decStockAmount mVarConn stockId = withMVar mVarConn $ \conn -> do
  execute conn "UPDATE stock SET amount = amount - 1 WHERE id = ? AND amount > 0" [stockId]
  affectedRows <- changes conn
  if affectedRows == 0
    then pure $ Left "Jemand anderes war leider schneller :'("
    else pure $ Right ()

prettyPrintStocks :: [Stock] -> IO ()
prettyPrintStocks ss = do
  print $ PP.black $ PP.ondullwhite $ PP.fill 5 (PP.text "ID") PP.<+> PP.fill 15 (PP.text "Label") PP.<+> PP.fill 6 (PP.text "Price") PP.<+> PP.fill 5 (PP.text "Amount")
  print $ PP.vcat $ fmap (\s ->
    PP.fill 5 (PP.int $ _stockId s) PP.<+>
    PP.fill 15 (PP.text $ _label s) PP.<+>
    PP.fill 6 (PP.text $ printf "%.2f€" $ _price s) PP.<+>
    PP.fill 5 (PP.int $ _amount s)
    ) ss
