module Beckn.Types.Core.Item where

import Beckn.Types.Amount (Amount)
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Duration
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Text,
    parent_item_id :: Maybe Text,
    descriptor :: Descriptor,
    price :: Price,
    model_id :: Maybe Text,
    category_id :: Maybe Text,
    package_category_id :: Maybe Text,
    brand_id :: Maybe Text,
    promotional :: Bool,
    ttl :: Maybe Duration,
    tags :: [Tag],
    -- Not in spec
    discount :: Maybe Amount
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Item where
  example =
    Item
      { id = idExample,
        parent_item_id = Just idExample,
        descriptor = example,
        price = example,
        model_id = Just idExample,
        brand_id = Just idExample,
        category_id = Just idExample,
        package_category_id = Just idExample,
        tags = example,
        promotional = False,
        ttl = Nothing,
        discount = Nothing
      }
