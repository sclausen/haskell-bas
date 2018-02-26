{-# LANGUAGE LambdaCase #-}


module Repl (
  Repl
  , makeSettings
  , process
  , repl
  , keywords
  )
  where

import           Control.Concurrent.MVar
import           Control.Monad.Trans      (lift, liftIO)
import           Data.Char
import           Data.List
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import           PrettyPrint
import           System.Console.Haskeline
import           System.Exit
import           Text.Printf
import           Text.Read                (readMaybe)

type Repl a = InputT IO a

makeSettings :: IO (Settings IO)
makeSettings = pure $ (defaultSettings :: Settings IO)
  { historyFile = Nothing
  , complete = completeWord Nothing " \t" $ return . search
  }

keywords :: [String]
keywords = ["buy", "debts", "exit", "help", "purchases", "stocks"]

search :: String -> [Completion]
search str = simpleCompletion <$> filter (str `isPrefixOf`) keywords

process :: String -> Storage -> Repl ()
process s storage
  | s == "" = return ()
  | s == "buy"                      = buy storage
  | s == "help"                     = liftIO $ putStrLn $ "commands: " ++ unwords keywords
  | s `elem` ["stocks", "ls", "ll"] = liftIO $ stocks storage
  | s == "purchases"                = liftIO $ purchases storage
  | s == "debts"                    = liftIO $ debts storage
  | s `elem` ["q", "quit", "exit"]  = liftIO exitSuccess
  | otherwise                       = return ()

repl :: Storage -> Repl ()
repl storage = do
  prettyPrompt <- lift $ getPrettyPrompt storage
  minput <- getInputLine prettyPrompt
  case minput of
    Nothing    -> liftIO exitSuccess
    Just input -> process (trim input) storage >> repl storage

debts :: Storage -> IO ()
debts storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> readMVar currentUser >>= \u-> putStrLn $ prettyDebts (_debts u == 0) $ printf "%.2f€" (fromIntegral (_debts u) / 100 :: Float)

purchases :: Storage -> IO ()
purchases storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> printPurchases storage 10

stocks :: Storage -> IO ()
stocks storage = prettyPrintStocks =<< _fetchStocks storage

buy :: Storage -> Repl ()
buy storage =
  lift (isEmptyMVar (_currentUser storage)) >>= \case
    True -> outputStrLn (errorText "You're not logged in")
    False -> do
      liftIO $ stocks storage
      getInputLine "\nPlease enter a StockId: " >>= \case
        Nothing -> liftIO exitSuccess
        Just input -> liftIO $
          case readStockId input of
            Nothing -> putStrLn (errorText "This is not a valid StockId")
            Just stockId -> _decAndFetchStock storage stockId >>= \case
              Nothing -> putStrLn (errorText $ "No Stock exists under the StockId " ++ show stockId :: String)
              Just stock -> do
                _incUserDebts storage (_price stock)
                _addPurchase storage stockId
                putStrLn $ successText $ "You bought one item of the stock \""++ _label stock ++ "\" for " ++ printf "%.2f€" (fromIntegral (_price stock) / 100 :: Float) ++ "."

  where
    readStockId :: String -> Maybe StockId
    readStockId = readMaybe

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
