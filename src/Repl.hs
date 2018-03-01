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
keywords isAdmin = sort $ ["buy", "debts", "exit", "help", "payments", "purchases", "stocks"] ++ if isAdmin then ["pay", "users"] else []

search :: Bool -> String -> [Completion]
search isAdmin str = simpleCompletion <$> filter (str `isPrefixOf`) (keywords isAdmin)

process :: String -> Storage -> Repl ()
process s storage = do
  user <- liftIO $ readMVar $ _currentUser storage
  if _userIsAdmin user
    then processAdmin s storage
    else processUser s storage

processAdmin :: String -> Storage -> Repl ()
processAdmin s storage
  | s == "" = return ()
  | s == "pay"           = pay storage
  | s == "users"         = liftIO $ users storage
  | s == "users add"     = startAddUser storage
  | s == "users update"  = startUpdateUser storage
  | s == "users delete"  = startDeleteUser storage
  | s == "stocks add"    = startAddStock storage
  | s == "stocks update" = startUpdateStock storage
  | otherwise            = processUser s storage

processUser :: String -> Storage -> Repl ()
processUser s storage
  | s == ""                         = return ()
  | s == "buy"                      = buy storage
  | s == "help"                     = showHelp
  | s `elem` ["stocks", "ls", "ll"] = liftIO $ stocks storage
  | s == "payments"                 = liftIO $ payments storage
  | s == "purchases"                = liftIO $ purchases storage
  | s == "debts"                    = liftIO $ debts storage
  | s `elem` ["q", "quit", "exit"]  = liftIO exitSuccess
  | otherwise                       = showHelp
    where
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
debts storage = readMVar (_currentUser storage) >>= \u-> putStrLn $ prettyDebts (_userDebts u == 0) $ printf "%.2f€" (fromIntegral (_userDebts u) / 100 :: Float)

purchases :: Storage -> IO ()
purchases storage = printPurchases storage 10

payments :: Storage -> IO ()
payments storage = printPayments storage 10

stocks :: Storage -> IO ()
stocks storage = prettyPrintStocks =<< _fetchStocks storage

users :: Storage -> IO ()
users storage = prettyPrintUsers =<< _fetchUsers storage

startAddUser :: Storage -> Repl ()
startAddUser storage = getInputLine "username: " >>= \case
  Nothing -> liftIO exitSuccess
  Just username -> getInputLine "isAdmin (y/N): " >>= \case
    Nothing -> liftIO exitSuccess
    Just isAdminInput -> liftIO (_addUser storage $ User 0 username 0 (yToBool isAdminInput)) >>= \case
      True  -> liftIO $ putStrLn $ successText ("The user \"" ++ username ++ "\" has successfully been added!")
      False -> liftIO $ putStrLn $ errorText   ("The user \"" ++ username ++ "\" already exists!")

startDeleteUser :: Storage -> Repl ()
startDeleteUser storage = getInputLine "username: " >>= \case
  Nothing -> liftIO exitSuccess
  Just username -> liftIO (_deleteUser storage username) >>= \case
    True  -> liftIO $ putStrLn $ successText ("The user \"" ++ username ++ "\" has successfully been deleted!")
    False -> liftIO $ putStrLn $ errorText   ("The user \"" ++ username ++ "\" doesn't exists!")

startAddStock :: Storage -> Repl ()
startAddStock storage = do
  user <- liftIO $ readMVar $ _currentUser storage
  liftIO $ print user

startUpdateUser :: Storage -> Repl ()
startUpdateUser storage = do
  liftIO $ users storage
  getInputLine "Please enter a UserId: " >>= \case
    Nothing -> liftIO exitSuccess
    Just input -> case readUserId input of
        Nothing -> liftIO $ putStrLn (errorText "This is not a valid UserId")
        Just userId -> getInputLine "Should the user be admin? (y/N): " >>= \case
          Nothing -> liftIO exitSuccess
          Just isAdminInput -> liftIO $ _updateUser storage userId (yToBool isAdminInput) >>= \case
            False -> putStrLn (errorText $ "No stock exists under the StockId " ++ show userId :: String)
            True -> putStrLn (successText "The user has been updated!" :: String)

startUpdateStock :: Storage -> Repl ()
startUpdateStock storage = do
  liftIO $ stocks storage
  getInputLine "Please enter a StockId: " >>= \case
    Nothing -> liftIO exitSuccess
    Just input -> case readStockId input of
        Nothing -> liftIO $ putStrLn (errorText "This is not a valid StockId")
        Just stockId -> getInputLine "Please enter an new amount to set: " >>= \case
          Nothing -> liftIO exitSuccess
          Just amountInput -> liftIO $
            case readInt amountInput of
              Nothing -> putStrLn (errorText "This is not a valid amount!")
              Just amount -> _updateStock storage stockId amount >>= \case
                False -> putStrLn (errorText $ "No stock exists under the StockId " ++ show stockId :: String)
                True -> putStrLn (successText $ "The amount of the stock with the StockId " ++ show stockId ++ " has been set to " ++ show amount :: String)

pay :: Storage -> Repl ()
pay storage = do
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
              case readInt amountInput of
                Nothing -> putStrLn (errorText "This is not a valid amount to pay in")
                Just amount -> do
                  _incUserDebts storage userId $ negate amount
                  _addPayment storage userId amount
                  putStrLn $ successText $ "You recoreded a payment of " ++ printf "%.2f€" (fromIntegral amount / 100 :: Float) ++ " for the user \"" ++ _userName user ++ "\"."

buy :: Storage -> Repl ()
buy storage = do
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

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

readStockId :: String -> Maybe StockId
readStockId = readMaybe

readInt :: String -> Maybe Int
readInt = readMaybe

readUserId :: String -> Maybe UserId
readUserId = readMaybe

yToBool :: String -> Bool
yToBool str = case toLower <$> str of
  "y" -> True
  _   -> False
