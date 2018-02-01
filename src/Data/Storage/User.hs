{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
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
  { _id        :: !UserId
  , _username  :: Username
  , _publicKey :: String
  , _debts     :: !Float
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

fetchUser :: MVar Connection -> String -> IO (Maybe User)
fetchUser mVarConn username = withMVar mVarConn $ queryDb
  where
    queryDb conn = do
      users <-  query conn "SELECT id, username, publicKey, debts from user WHERE username = ?" [username] :: IO [User]
      case users of
        []      -> pure Nothing
        [!user] -> pure $ Just user
        (_:_)   -> pure Nothing -- Should never happen, since the username is a primary key

incUserDebts :: MVar Connection -> Username -> Float -> IO ()
incUserDebts mVarConn username summand = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET debts = debts + ? WHERE username = ?" (summand, username)
