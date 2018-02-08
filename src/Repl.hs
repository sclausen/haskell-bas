{-# LANGUAGE LambdaCase #-}


module Repl (
  Repl
  , makeSettings
  , process
  , repl
  )
  where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans      (lift, liftIO)
import           Data.Char
import           Data.List
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import           PrettyPrint
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           Text.Printf
import           Text.Read                (readMaybe)

type Repl a = InputT IO a

makeSettings :: IO (Settings IO)
makeSettings = do
  hf <- getEnv "HISTORY_FILE"
  pure $ (defaultSettings :: Settings IO)
    { historyFile = Just hf
    , complete = completeWord Nothing " \t" $ return . search
    }

keywords :: [String]
keywords = ["buy", "debts", "exit", "purchases", "stocks"]

search :: String -> [Completion]
search str = simpleCompletion <$> filter (str `isPrefixOf`) keywords

process :: String -> Storage -> Repl ()
process s storage
  | s == "" = return ()
  | s == "buy"                     = buy storage
  | s == "stocks"                  = liftIO $ stocks storage
  | s == "purchases"               = liftIO $ purchases storage
  | s == "debts"                   = liftIO $ debts storage
  | s `elem` ["q", "quit", "exit"] = liftIO exitSuccess
  | otherwise                      = return ()

repl :: Storage -> Repl ()
repl storage = do
  prettyPrompt <- lift $ getPrettyPrompt storage
  minput <- getInputLine prettyPrompt
  case minput of
    Nothing    -> outputStrLn (errorText "Never heard of really empty input…")
    Just input -> process (trim input) storage >> repl storage

debts :: Storage -> IO ()
debts storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> readMVar currentUser >>= \u-> putStrLn $ prettyDebts $ printf "%.2f€" (_debts u)

purchases :: Storage -> IO ()
purchases storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> _fetchPurchases storage 0 10

stocks :: Storage -> IO ()
stocks storage = prettyPrintStocks =<< _fetchStocks storage

buy :: Storage -> Repl ()
buy storage =
  lift (isEmptyMVar (_currentUser storage)) >>= \case
    True -> outputStrLn (errorText "You're not logged in")
    False -> do
      currentUser <- lift $ readMVar (_currentUser storage)
      lift (_fetchUser storage (_username currentUser)) >>= \case
        Nothing -> outputStrLn (errorText "the user wasn't found")
        Just user ->
          getInputLine "Please enter a StockId: " >>= \case
            Nothing -> outputStrLn "How can you enter nothing? Would be strange, wouldn't it?"
            Just input -> liftIO $
              case readStockId input of
                Nothing -> putStrLn (errorText "This is not a valid StockId")
                Just stockId -> _decStockAmount storage stockId >>= \case
                  Left _ -> putStrLn (errorText $ "No Stock exists under the StockId " ++ show stockId :: String)
                  Right stock -> do
                    _incUserDebts storage (_userId user) (_price stock)
                    _addPurchase storage (_userId user) stockId
                    _fetchUser storage (_username currentUser) >>= \case
                      Nothing   -> putStrLn (errorText "The user has been deleted")
                      Just u -> do
                        void $ swapMVar (_currentUser storage) u
                        putStrLn $ successText $ "You've bought one item of the stock \""++ _label stock ++ "\" for " ++ printf "%.2f€" (_price stock) ++ "."

  where
    readStockId :: String -> Maybe StockId
    readStockId = readMaybe

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
