{-# LANGUAGE OverloadedStrings #-}

module Repl (
  Repl
  , appSettings
  , process
  , repl
  )
  where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans
import           Data.List
import           Stock
import           System.Console.Haskeline
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type Repl a = InputT IO a

appSettings :: Settings IO
appSettings = (defaultSettings :: Settings IO)
  { historyFile = Just "history"
  , complete = completeWord Nothing " \t" $ return . search
  }

keywords :: [String]
keywords = ["stocks", "buy", "me"]

search :: String -> [Completion]
search str = map simpleCompletion $ filter (str `isPrefixOf`) keywords

process :: String -> MVar Stock -> IO ()
process s mStock
  | s == "" = return ()
  | s == "me" = return ()
  | s == "stocks" = do
    stock <- readMVar mStock
    prettyPrint stock
  | s `elem` ["h", "help"]         = putStr helpText
  | s `elem` ["q", "quit", "exit"] = exitSuccess
  | otherwise                      = putStr helpText
  where
    helpText = "help text\n"

repl :: MVar Stock -> Repl ()
repl stock = do
  minput <- getInputLine "bas $ "
  case minput of
    Nothing    -> outputStrLn "This should not happen."
    Just input -> liftIO (process input stock) >> repl stock

prettyPrint :: Stock -> IO ()
prettyPrint s = do
  print $ black $ ondullwhite $ fill 10 (text "Product") PP.<+> fill 6 (text "Price") PP.<+> fill 6 (text "Stock")
  print $ vcat $ fmap (\(label, price, amount)->
    white $ ondullblack $ label PP.<+> price PP.<+> amount
    ) (items' s)
  where
    items' s = fmap (\item -> (fill 10 $ text $ _label item, fill 6 $ float $ _price item, fill 6 $ int $ _amount item)) (_items s)
