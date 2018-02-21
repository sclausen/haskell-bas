{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.User (
    User (..)
  , UserId
  , fetchUser
  , incUserDebts
) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Database.SQLite.Simple

type UserId = Int
type Username = String

data User = User
  { _userId   :: !UserId
  , _username :: Username
  , _debts    :: !Int
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

fetchUser :: MVar Connection -> String -> IO (Maybe User)
fetchUser mVarConn username = withMVar mVarConn $ \conn -> query conn "SELECT id, username, debts from user WHERE username = ?" [username] >>= \case
  [!user] -> pure $ Just user
  _      -> pure Nothing

incUserDebts :: MVar Connection -> MVar User -> Int -> IO ()
incUserDebts mVarConn mUser summand = do
  user <- readMVar mUser
  withMVar mVarConn $ \conn -> execute conn "UPDATE user SET debts = debts + ? WHERE id = ?" (summand, _userId user)
  fetchUser mVarConn (_username user) >>= \case
    Just user' -> void $ swapMVar mUser user'
    Nothing    -> pure ()
