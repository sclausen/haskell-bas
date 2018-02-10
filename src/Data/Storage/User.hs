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
import           Database.SQLite.Simple

type UserId = Int
type Username = String

data User = User
  { _userId   :: !UserId
  , _username :: Username
  , _debts    :: !Float
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

fetchUser :: MVar Connection -> String -> IO (Maybe User)
fetchUser mVarConn username = withMVar mVarConn $ \conn -> query conn "SELECT id, username, debts from user WHERE username = ?" [username] >>= \case
  [!user] -> pure $ Just user
  _      -> pure Nothing

incUserDebts :: MVar Connection -> UserId -> Float -> IO ()
incUserDebts mVarConn userId summand = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET debts = debts + ? WHERE id = ?" (summand, userId)
