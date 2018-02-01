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
import           Control.Monad.Trans          (liftIO)
import           Data.List
import           Data.Storage.Stock
import           Data.Storage.Storage
import           Data.Storage.User
import           System.Console.Haskeline
import           System.Exit
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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

process :: String -> Storage -> IO ()
process s storage
  | s == "" = return ()
  -- | s == "stocks" = do
  --   _storage
  | "buy" `isPrefixOf` s           = buy storage s
  | s `elem` ["stocks"]            = stocks storage
  | s `elem` ["purchases"]         = purchases storage
  | s `elem` ["me"]                = me storage
  | "login" `isPrefixOf` s         = login storage s
  | s `elem` ["logout"]            = logout storage
  | s `elem` ["h", "help"]         = putStr helpText
  | s `elem` ["q", "quit", "exit"] = exitSuccess
  | otherwise                      = putStr helpText
  where
    helpText = show $  PP.black $ PP.ondullwhite $ PP.text "help text" PP.<$> PP.softbreak

repl :: Storage -> Repl ()
repl storage = do
  minput <- getInputLine "bas $ "
  case minput of
    Nothing    -> outputStrLn "This should not happen."
    Just input -> liftIO (process input storage) >> (repl storage)

printMore :: Double -> IO()
printMore percentage = print $ PP.black $ PP.ondullwhite $ PP.text ("--More--(" ++ show percentage ++ "%) ")

login :: Storage -> String -> IO ()
login storage s = do
  let currentUser = (_currentUser storage)
  isEmptyMVar currentUser >>= \case
    False -> putStrLn "You're already logged in"
    True -> _fetchUser storage (last $ words s) >>= \case
      Nothing -> putStrLn "This user doesn't exist!"
      Just user -> putMVar (_currentUser storage) user >> putStrLn "Successfully logged in!"

logout :: Storage -> IO ()
logout storage = do
  let currentUser = (_currentUser storage)
  isEmptyMVar currentUser >>= \case
    True -> putStrLn "You're not logged in"
    False -> ((\_-> putStrLn "You've been successfully logged out!") =<< takeMVar currentUser)

me :: Storage -> IO ()
me storage = do
  let currentUser = _currentUser storage
  isEmptyMVar currentUser >>= \case
    True -> putStrLn "You're not logged in"
    False -> readMVar currentUser >>= (\u-> putStrLn $ "You're logged in as \"" ++ (_username u) ++ "\" And youre " ++ show (_debts u) ++ "€ in debt.")

purchases :: Storage -> IO ()
purchases storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn "You're not logged in"
    False -> putStrLn "List of all your purchases here ->"

stocks :: Storage -> IO ()
stocks storage = _fetchStocks storage >>= print

buy :: Storage -> String -> IO ()
buy storage s =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn "You're not logged in"
    False -> do
      currentUser <- readMVar (_currentUser storage)
      _fetchUser storage (_username currentUser) >>= \case
        Nothing -> putStrLn "the user wasn't found"
        Just user -> do
          let stockId = readStockId $ last $ words s
          _decStockAmount storage stockId >>= \case
            Left _ -> putStrLn ("Something weird happened, Ladies and Gentlemen!" :: String)
            Right _ ->
              _fetchStock storage stockId >>= \case
                Just stock -> do
                  let userId = (_userId user)
                  _incUserDebts storage userId (_price stock)
                  _addPurchase storage userId stockId
                  newUser <- _fetchUserUnsafe storage (_username currentUser)
                  _ <- swapMVar (_currentUser storage) newUser
                  pure ()
                Nothing -> print ("Something weird happened, Ladies and Gentlemen!" :: String)
  where
    readStockId :: String -> StockId
    readStockId = read
