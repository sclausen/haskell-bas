{-# LANGUAGE LambdaCase #-}


module Repl (
  Repl
  , appSettings
  , process
  , repl
  , printMore
  )
  where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans          (lift, liftIO)
import           Data.Char
import           Data.List
import           Data.Storage.Purchase
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import           System.Console.Haskeline
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf

type Repl a = InputT IO a

appSettings :: Settings IO
appSettings = (defaultSettings :: Settings IO)
  { historyFile = Just "history"
  , complete = completeWord Nothing " \t" $ return . search
  }

keywords :: [String]
keywords = ["stocks", "buy", "login", "logout", "me", "purchases"]

search :: String -> [Completion]
search str = map simpleCompletion $ filter (str `isPrefixOf`) keywords

process :: String -> Storage -> Repl ()
process s storage
  | s == "" = return ()
  | s =="buy"                      = buy storage
  | s == "stocks"                  = liftIO $ stocks storage
  | s == "purchases"               = liftIO $ purchases storage
  | s == "me"                      = liftIO $ me storage
  | s == "login"                   = login storage
  | s == "logout"                  = liftIO $ logout storage
  | s `elem` ["q", "quit", "exit"] = liftIO exitSuccess
  | otherwise                      = outputStr helpText
  where
    helpText = show $  PP.black $ PP.ondullwhite $ PP.text "help text" PP.<$> PP.softbreak

repl :: Storage -> Repl ()
repl storage = do
  minput <- getInputLine "bas $ "
  case minput of
    Nothing    -> outputStrLn "This should not happen."
    Just input -> process (trim input) storage >> repl storage

printMore :: Double -> IO()
printMore percentage = print $ PP.black $ PP.ondullwhite $ PP.text ("--More--(" ++ show percentage ++ "%) ")

login :: Storage -> Repl ()
login storage = do
  let currentUser = _currentUser storage
  lift (isEmptyMVar currentUser) >>= \case
    False -> outputStrLn "You're already logged in"
    True -> getInputLine "Please enter a username: " >>= \case
      Nothing -> outputStrLn "Never heard of really empty input…"
      Just username -> lift (_fetchUser storage username >>= \case
        Nothing -> putStrLn "This user doesn't exist!"
        Just user -> putMVar (_currentUser storage) user >> putStrLn "Successfully logged in!")

logout :: Storage -> IO ()
logout storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> putStrLn "You're not logged in"
    False -> takeMVar currentUser >> putStrLn "You've been successfully logged out!"

me :: Storage -> IO ()
me storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> putStrLn "You're not logged in"
    False -> readMVar currentUser >>= (\u-> putStrLn $ "You're logged in as \"" ++ _username u ++ "\" and you're " ++ printf "%.2f€" (_debts u) ++ " in debt.")

purchases :: Storage -> IO ()
purchases storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn "You're not logged in"
    False -> prettyPrintPurchase =<< _fetchPurchases storage 0 10

stocks :: Storage -> IO ()
stocks storage = prettyPrintStocks =<< _fetchStocks storage

buy :: Storage -> Repl ()
buy storage =
  lift (isEmptyMVar (_currentUser storage)) >>= \case
    True -> outputStrLn "You're not logged in"
    False -> do
      currentUser <- lift $ (readMVar (_currentUser storage))
      lift (_fetchUser storage (_username currentUser)) >>= \case
        Nothing -> outputStrLn "the user wasn't found"
        Just user -> do
          mLine <- getInputLine "Please enter a StockId: "
          case mLine of
            Nothing -> outputStrLn "How can you enter nothing? Would be strange, wouldn't it?"
            Just input -> liftIO $ do
              let stockId = readStockId input
              _decStockAmount storage stockId >>= \case
                Left _ -> putStrLn ("No Stock exists under the StockId " ++ show stockId :: String)
                Right _ ->
                  _fetchStock storage stockId >>= \case
                    Just stock -> do
                      let userId = _userId user
                      _incUserDebts storage userId (_price stock)
                      _addPurchase storage userId stockId
                      _fetchUser storage (_username currentUser) >>= \case
                        Nothing   -> putStrLn "The user has been deleted"
                        Just u -> do
                          void $ swapMVar (_currentUser storage) u
                          putStrLn $ "You've bought one item of the stock \""++ _label stock ++ "\" for " ++ printf "%.2f€" (_price stock) ++"."
                    Nothing -> putStrLn ("Something weird happened, Ladies and Gentlemen!" :: String)
  where
    readStockId :: String -> StockId
    readStockId = read

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
