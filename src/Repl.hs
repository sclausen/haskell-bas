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
import           Control.Monad
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

makeSettings :: MVar User -> IO (Settings IO)
makeSettings mUser = do
  currentUser <- readMVar mUser
  pure $ (defaultSettings :: Settings IO)
    { historyFile = Nothing
    , complete = completeWord Nothing " \t" $ return . search (_userIsAdmin currentUser)
    }

keywords :: Bool -> [String]
keywords isAdmin = sort $ ["buy", "debts", "exit", "help", "payments", "purchases", "stocks"] ++ ["pay" | isAdmin]

search :: Bool -> String -> [Completion]
search isAdmin str = simpleCompletion <$> filter (str `isPrefixOf`) (keywords isAdmin)

process :: String -> Storage -> Repl ()
process s storage
  | s == "" = return ()
  | s == "buy"                      = buy storage
  | s == "help"                     = showHelp
  | s `elem` ["stocks", "ls", "ll"] = liftIO $ stocks storage
  | s == "pay"                      = startPay
  | s == "payments"                 = liftIO $ payments storage
  | s == "purchases"                = liftIO $ purchases storage
  | s == "debts"                    = liftIO $ debts storage
  | s `elem` ["q", "quit", "exit"]  = liftIO exitSuccess
  | otherwise                       = showHelp
    where
      startPay = do
        currentUser <- liftIO $ readMVar (_currentUser storage)
        if _userIsAdmin currentUser then pay storage else showHelp
      showHelp = do
        currentUser <- liftIO $ readMVar (_currentUser storage)
        liftIO $ putStrLn $ "commands: " ++ unwords (keywords (_userIsAdmin currentUser))

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
    False -> readMVar currentUser >>= \u-> putStrLn $ prettyDebts (_userDebts u == 0) $ printf "%.2f€" (fromIntegral (_userDebts u) / 100 :: Float)

purchases :: Storage -> IO ()
purchases storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> printPurchases storage 10

payments :: Storage -> IO ()
payments storage =
  isEmptyMVar (_currentUser storage) >>= \case
    True -> putStrLn (errorText "You're not logged in")
    False -> printPayments storage 10

stocks :: Storage -> IO ()
stocks storage = prettyPrintStocks =<< _fetchStocks storage

users :: Storage -> IO ()
users storage = prettyPrintUsers =<< _fetchUsers storage

pay :: Storage -> Repl ()
pay storage =
  lift (isEmptyMVar (_currentUser storage)) >>= \case
    True -> outputStrLn (errorText "You're not logged in")
    False -> do
      liftIO $ users storage
      getInputLine "Please enter a UserId: " >>= \case
        Nothing -> liftIO exitSuccess
        Just userInput -> case readUserId userInput of
          Nothing -> liftIO $ putStrLn (errorText "This is not a valid UserId")
          Just userId -> liftIO (_fetchUser storage userId) >>= \case
              Nothing -> liftIO $ putStrLn (errorText $ "No user exists under the UserId " ++ show userId :: String)
              Just user -> getInputLine "Please enter an amount to pay in: " >>= \case
                Nothing -> liftIO exitSuccess
                Just amountInput -> liftIO $
                  case readAmount amountInput of
                    Nothing -> putStrLn (errorText "This is not a valid amount to pay in")
                    Just amount -> do
                      _incUserDebts storage userId $ negate amount
                      _addPayment storage userId amount
                      putStrLn $ successText $ "You recoreded a payment of " ++ printf "%.2f€" (fromIntegral amount / 100 :: Float) ++ " for the user \"" ++ _userName user ++ "\"."

  where
    readUserId :: String -> Maybe UserId
    readUserId = readMaybe
    readAmount :: String -> Maybe Int
    readAmount = readMaybe

buy :: Storage -> Repl ()
buy storage =
  lift (isEmptyMVar (_currentUser storage)) >>= \case
    True -> outputStrLn (errorText "You're not logged in")
    False -> do
      user <- liftIO $ readMVar $ _currentUser storage
      liftIO $ stocks storage
      getInputLine "Please enter a StockId: " >>= \case
        Nothing -> liftIO exitSuccess
        Just input -> liftIO $
          case readStockId input of
            Nothing -> putStrLn (errorText "This is not a valid StockId")
            Just stockId -> _decAndFetchStock storage stockId >>= \case
              Nothing -> putStrLn (errorText $ "No stock exists under the StockId " ++ show stockId :: String)
              Just stock -> do
                _incUserDebts storage (_userId user) (_stockPrice stock)
                _fetchUser storage (_userId user) >>= \case
                  Just user' -> void $ swapMVar (_currentUser storage) user'
                  Nothing    -> pure ()
                _addPurchase storage stockId
                putStrLn $ successText $ "You bought one item of the stock \""++ _stockLabel stock ++ "\" for " ++ printf "%.2f€" (fromIntegral (_stockPrice stock) / 100 :: Float) ++ "."

  where
    readStockId :: String -> Maybe StockId
    readStockId = readMaybe

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
