-- module Beckn.Types.Core.Migration.Item where
module Core.Item where

import Core.Descriptor
import Core.Price
-- import Beckn.Types.Core.Migration.Tags (Tags)
-- import Beckn.Types.Core.Migration.Time (Time)
-- import EulerHS.Prelude hiding (id)
import Data.Aeson
import Data.Text
import GHC.Generics

data Item = Item
  { id :: Maybe Text,
    -- parent_item_id :: Maybe Text,
    fulfillment_id :: Maybe Text,
    descriptor :: Maybe Descriptor,
    price :: Maybe Price,
    quantity :: Maybe Quantity

    -- category_id :: Maybe Text,
    -- location_id :: Maybe Text,
    -- time :: Maybe Time,
    -- matched :: Maybe Bool,
    -- related :: Maybe Bool,
    -- recommended :: Maybe Bool,
    -- tags :: Maybe Tags
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Item where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Quantity = Quantity 
  { count :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show)