{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Storage.User (
    User (..)
  , fetchUser
  , incDepts
) where

import           Control.Concurrent.MVar
import           Database.SQLite.Simple

data User = User
  { _id        :: !Int
  , _username  :: String
  , _publicKey :: String
  , _depts     :: !Float
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
    queryDb _ "" = pure Nothing
    queryDb conn username = do
      users <-  query conn "SELECT id, username, public_key, depts from user WHERE username = ?" [username] :: IO [User]
      case users of
        []      -> pure Nothing
        [!user] -> pure $ Just user
        (_:_)   -> pure Nothing -- Should never happen, since the username is a primary key

incDepts :: MVar Connection -> String -> Float -> IO ()
incDepts mVarConn username factor = withMVar mVarConn $ \conn -> execute conn "UPDATE user SET depts = depts + ? WHERE username = ?" (factor, username)
