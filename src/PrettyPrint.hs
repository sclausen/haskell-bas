{-# LANGUAGE LambdaCase #-}
module PrettyPrint (
    getPrettyPrompt
  , prettyDebts
  , prettyUsername
  , prettyPrintStocks
  , printPurchases
  , errorText
  , successText
) where

import qualified Control.Applicative          as A
import           Control.Concurrent.MVar
import           Control.Monad
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
    fill 15 (text $ _label s) <+>
    fill 6 (text $ printf "%.2f€" $ _price s) <+>
    fill 5 (int $ _amount s)
    ) ss

getPrettyPrompt :: Storage -> IO String
getPrettyPrompt storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> pure "bas % "
    False -> readMVar currentUser >>= \u -> pure $ boldBlueText (_username u) ++ "@bas % "

boldBlueText :: String -> String
boldBlueText = show . bold . blue . text

prettyUsername :: String -> String
prettyUsername = show . bold . blue . text

prettyDebts :: String -> String
prettyDebts = show . bold . red . text

errorText :: String -> String
errorText = show . red . text

successText :: String -> String
successText = show . green . text

printPurchases :: Storage -> Int -> IO ()
printPurchases storage limit = do
  t <- _fetchPurchases storage 0 1
  when (null t) (print $ red (text "Sorry, you haven't bought anything yet!"))
  unless (null t) printHeader
  printPurchases' 0
  where
    printPurchases' curOffset = _fetchPurchases storage curOffset limit >>= \case
      []        -> pure ()
      ps -> do
        prettyPrintPurchases ps
        getCharHidden (printPurchases' (curOffset + 1))

printHeader :: IO ()
printHeader =
  print
    $   bold
    $   black
    $   ondullwhite
    $   fill 15 (text "Product")
    <+> fill 6  (text "Price")
    <+> fill 23 (text "Date")

prettyPrintPurchases :: [Purchase] -> IO ()
prettyPrintPurchases ps = print $ vcat $ fmap
  ( \p ->
    fill 15 (text $ _stockLabel p) <+>
    fill 6  (text $ printf "%.2f€" $ _sPrice p) <+>
    fill 23 (text $ show $ _boughtAt p)
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
