{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Storage.User (
    User (..)
  , UserId
  , Username
  , addUser
  , updateUser
  , deleteUser
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

instance ToRow User where
  toRow (User _userId _userName _userDebts _userIsAdmin) = toRow (_userId, _userName, _userDebts, _userIsAdmin)

addUser :: MVar Connection -> User -> IO Bool
addUser mConn user = do
  let username = _userName user
  userExists <- doesUserExists mConn username
  if userExists
    then pure False
    else do
      withMVar mConn $ \conn -> execute conn "INSERT INTO user (username, debts, isAdmin) VALUES (?, 0, ?)" (username, _userIsAdmin user)
      pure True

updateUser :: MVar Connection -> UserId -> Bool -> IO Bool
updateUser mConn userId isAdmin = withMVar mConn $ \conn -> do
  execute conn "UPDATE user SET isAdmin = ? WHERE id = ?" (isAdmin, userId)
  (/= 0) <$> changes conn

deleteUser :: MVar Connection -> Username -> IO Bool
deleteUser mConn username = do
  userExists <- doesUserExists mConn username
  if userExists
    then do
      withMVar mConn $ \conn -> execute conn "DELETE FROM user WHERE username = ?" (Only username)
      pure True
    else
      pure False

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

doesUserExists :: MVar Connection -> Username -> IO Bool
doesUserExists mVarConn username = withMVar mVarConn $ \conn -> (query conn "SELECT id, username, debts, isAdmin from user WHERE username = ?" [username] :: IO [User]) >>= \case
  [_] -> pure True
  _   -> pure False
