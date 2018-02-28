{-# LANGUAGE LambdaCase #-}
module PrettyPrint (
    getPrettyPrompt
  , prettyDebts
  , prettyUsername
  , prettyPrintStocks
  , printPayments
  , printPurchases
  , errorText
  , successText
) where

import qualified Control.Applicative          as A
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Storage.Payment
import           Data.Storage.Purchase
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import           System.IO                    (hReady, hSetEcho, stdin)
import           Text.PrettyPrint.ANSI.Leijen
import           Text.Printf

prettyPrintStocks :: [Stock] -> IO ()
prettyPrintStocks ss = do
  print $ bold $ black $ ondullwhite $ fill 5 (text "ID") <+> fill 15 (text "Label") <+> fill 6 (text "Price") <+> fill 5 (text "Left")
  print $ vcat $ fmap (\s ->
    fill 5 (int $ _stockId s) <+>
    fill 15 (text $ _stockLabel s) <+>
    fill 6 (text $ printf "%.2f€" (fromIntegral (_stockPrice s) / 100 :: Float)) <+>
    fill 5 (int $ _stockAmount s)
    ) ss

getPrettyPrompt :: Storage -> IO String
getPrettyPrompt storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> pure "bas % "
    False -> readMVar currentUser >>= \u -> pure $ boldBlueText (_userName u) ++ "@bas % "

boldBlueText :: String -> String
boldBlueText = show . bold . blue . text

prettyUsername :: String -> String
prettyUsername = show . bold . blue . text

prettyDebts :: Bool -> String -> String
prettyDebts isZero str = show $ bold $ (if isZero then green else red) $ text str

errorText :: String -> String
errorText = show . red . text

successText :: String -> String
successText = show . green . text

printPurchases :: Storage -> Int -> IO ()
printPurchases storage limit = do
  t <- _fetchPurchases storage 0 1
  when (null t) (print $ red (text "Sorry, you haven't bought anything yet!"))
  unless (null t) printPurchasesHeader
  printPurchases' 0
  where
    printPurchases' curOffset = _fetchPurchases storage curOffset limit >>= \case
      []        -> pure ()
      ps -> do
        prettyPrintPurchases ps
        getCharHidden (printPurchases' (curOffset + 1))


printPayments :: Storage -> Int -> IO ()
printPayments storage limit = do
  let mUser = _currentUser storage
  t <- _fetchPayments storage mUser 0 1
  when (null t) (print $ red (text "Sorry, you haven't paid anything yet!"))
  unless (null t) printPaymentsHeader
  printPayments' 0
  where
    printPayments' curOffset = _fetchPayments storage (_currentUser storage) curOffset limit >>= \case
        []        -> pure ()
        ps -> do
          prettyPrintPayments ps
          getCharHidden (printPayments' (curOffset + 1))

printPaymentsHeader :: IO ()
printPaymentsHeader =
  print
    $   bold
    $   black
    $   ondullwhite
    $   fill 23 (text "Date")
    <+> fill 6  (text "Amount")

printPurchasesHeader :: IO ()
printPurchasesHeader =
  print
    $   bold
    $   black
    $   ondullwhite
    $   fill 15 (text "Product")
    <+> fill 6  (text "Price")
    <+> fill 23 (text "Date")

prettyPrintPayments :: [Payment] -> IO ()
prettyPrintPayments ps = print $ vcat $ fmap
  ( \p ->
    fill 23 (text $ show $ _paymentPaidAt p) <+>
    fill 6  (text $ printf "%.2f€" (fromIntegral (_paymentAmount p) / 100 :: Float))
  )
  ps

prettyPrintPurchases :: [Purchase] -> IO ()
prettyPrintPurchases ps = print $ vcat $ fmap
  ( \p ->
    fill 15 (text $ _purchaseStockLabel p) <+>
    fill 6  (text $ printf "%.2f€" (fromIntegral (_purchasePrice p) / 100 :: Float)) <+>
    fill 23 (text $ show $ _purchaseBoughtAt p)
  )
  ps

getCharHidden :: IO () -> IO ()
getCharHidden cb = do
  hSetEcho stdin False
  key <- getKey
  case key of
    "\n" -> cb
    _    -> return ()

getKey :: IO String
getKey = reverse A.<$> getKey' ""
 where
  getKey' cs = do
    c <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (c : cs)
