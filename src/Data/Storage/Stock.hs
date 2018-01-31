{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.Storage.Stock (
  Stock (..)
) where

import           Database.SQLite.Simple

data Stock = Stock
  { _id    :: !Int
  , _label :: String
  , _price :: !Float
  }

instance FromRow Stock where
  fromRow = Stock <$> field <*> field <*> field
