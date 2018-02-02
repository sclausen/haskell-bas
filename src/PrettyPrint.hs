module PrettyPrint where

import           Data.Storage.Purchase
import           Data.Storage.Stock
import           Text.PrettyPrint.ANSI.Leijen
import           Text.Printf

prettyPrintStocks :: [Stock] -> IO ()
prettyPrintStocks ss = do
  print $ black $ ondullwhite $ fill 5 (text "ID") <+> fill 15 (text "Label") <+> fill 6 (text "Price") <+> fill 5 (text "Amount")
  print $ vcat $ fmap (\s ->
    fill 5 (int $ _stockId s) <+>
    fill 15 (text $ _label s) <+>
    fill 6 (text $ printf "%.2f€" $ _price s) <+>
    fill 5 (int $ _amount s)
    ) ss

prettyPrintPurchase :: [Purchase] -> IO ()
prettyPrintPurchase ps = do
  print $ black $ ondullwhite $ fill 15 (text "Product") <+> fill 6 (text "Price") <+> fill 23 (text "Date")
  print $ vcat $ fmap (\p ->
    fill 15 (text $ _stockLabel p) <+>
    fill 6 (text $ printf "%.2f€" $ _sPrice p) <+>
    fill 23 (text $ show $ _boughtAt p)
    ) ps

