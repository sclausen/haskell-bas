{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Stock (
    Stock (..)
  , Item (..)
  , readStock
) where

import qualified Data.ByteString.Char8 as B8
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import           Data.Yaml             (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Yaml             as Y
import           GHC.Generics          (Generic)
import           Lens.Micro.TH

data Item = Item
  { _label  :: String
  , _price  :: Float
  , _amount :: Int
  } deriving (Show,Generic)

instance ToJSON Item
instance FromJSON Item

data Stock = Stock
  { _items :: [Item]
  } deriving (Show,Generic)

instance ToJSON Stock
instance FromJSON Stock

test :: IO ()
test = do
  file <- B8.readFile "./stock.yml"
  let req = Y.decode file :: Maybe Stock
--  let req = Y.decode "{\"_items\":[{\"_label\": \"Red Bull\", \"_price\": 1, \"_amount\": 24}]}" :: Maybe Stock
  print req
  -- let stock = Stock { _items = [Item { _label = "Kong", _price = 0.25, _amount = 30}] }
  -- B8.putStrLn (Y.encode stock)

readStock :: IO Stock
readStock = do
  file <- B8.readFile "./stock.yml"
  let mStock = Y.decode file :: Maybe Stock
  pure $ fromJust mStock
