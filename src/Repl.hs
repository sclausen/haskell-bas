{-# LANGUAGE OverloadedStrings #-}

module Repl (
  Repl
  , appSettings
  , process
  , repl
  )
  where

import           Control.Monad.Trans
import           Data.List
import           System.Console.Haskeline
import           System.Exit

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

process :: String -> IO ()
process s
  | s == "" = return ()
  | s == "me" = return ()
  -- | s == "stocks" = do
  --   stock <- readMVar mStock
  --   prettyPrint stock
  | s `elem` ["h", "help"]         = putStr helpText
  | s `elem` ["q", "quit", "exit"] = exitSuccess
  | otherwise                      = putStr helpText
  where
    helpText = "help text\n"

repl :: Repl ()
repl = do
  minput <- getInputLine "bas $ "
  case minput of
    Nothing    -> outputStrLn "This should not happen."
    Just input -> liftIO (process input) >> repl