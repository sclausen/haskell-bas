{-# LANGUAGE LambdaCase #-}
module PrettyPrint (
    prettyDebts
  , prettyPrintStocks
  , getPrettyPrompt
  , errorText
  , successText
) where

import           Control.Concurrent.MVar
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
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

prettyDebts :: String -> String
prettyDebts = show . bold . red . text

errorText :: String -> String
errorText = show . red . text

successText :: String -> String
successText = show . green . text
