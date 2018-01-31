{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Storage.Purchase (
  Purchase (..)
) where

import           Database.SQLite.Simple

data Purchase = Purchase
  { _id       :: !Int
  , _userId   :: !Int
  , _stockId  :: !Int
  , _boughtAt :: !Int
  }

instance FromRow Purchase where
  fromRow = Purchase <$> field <*> field <*> field <*> field
