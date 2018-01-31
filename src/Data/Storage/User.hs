{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Storage.User (
    User (..)
  , fetchUser
  , incDebts
) where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

data User = User
  { _id        :: !Int
  , _username  :: String
  , _publicKey :: String
  , _debts     :: !Float
  } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

fetchUser :: MVar Connection
          -> String
          -> IO (Maybe User)
fetchUser mVarConn username = do
  conn <- takeMVar mVarConn
  mUser <- queryDb conn username
  putMVar mVarConn conn
  pure mUser
  where
    queryDb conn username = do
      users <-  query conn "SELECT id, username, public_key, debts from user WHERE username = ?" [username] :: IO [User]
      case users of
        []      -> pure Nothing
        [!user] -> pure $ Just user
        (_:_)   -> pure Nothing -- Should never happen, since the username is a primary key

incDebts :: MVar Connection -> String -> Float -> IO ()
incDebts mVarConn username factor = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET debts = debts + ? WHERE username = ?" (factor, username)
