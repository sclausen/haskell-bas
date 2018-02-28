{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.User (
    User (..)
  , UserId
  , fetchUser
  , fetchUserByName
  , fetchUsers
  , incUserDebts
) where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

type UserId = Int
type Username = String

data User = User
  { _userId      :: !UserId
  , _userName    :: Username
  , _userDebts   :: !Int
  , _userIsAdmin :: Bool
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

fetchUserByName :: MVar Connection -> String -> IO (Maybe User)
fetchUserByName mVarConn username = withMVar mVarConn $ \conn -> query conn "SELECT id, username, debts, isAdmin from user WHERE username = ?" [username] >>= \case
  [!user] -> pure $ Just user
  _       -> pure Nothing

fetchUser :: MVar Connection -> UserId -> IO (Maybe User)
fetchUser mVarConn userId = withMVar mVarConn $ \conn -> query conn "SELECT id, username, debts, isAdmin from user WHERE id = ?" [userId] >>= \case
  [!user] -> pure $ Just user
  _       -> pure Nothing

fetchUsers :: MVar Connection -> IO [User]
fetchUsers mVarConn = withMVar mVarConn $ query_ `flip` "SELECT id, username, debts, isAdmin FROM user ORDER BY id ASC"

incUserDebts :: MVar Connection -> UserId-> Int -> IO ()
incUserDebts mVarConn userId summand = do
  let qry = if summand >= 0
              then "UPDATE user SET debts = debts + ? WHERE id = ?"
              else "UPDATE user SET debts = debts - ? WHERE id = ?"
  withMVar mVarConn $ \conn -> execute conn qry (summand, userId)
