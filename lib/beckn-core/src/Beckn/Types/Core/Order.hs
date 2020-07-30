module Beckn.Types.Core.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.ItemQuantity
import Beckn.Types.Core.Payment
import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Text,
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _items :: [OrderItem],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Order where
  example =
    Order
      { _id = idExample,
        _state = "State",
        _created_at = example,
        _updated_at = example,
        _items = example,
        _billing = example,
        _payment = example
      }

data OrderItem = OrderItem
  { _id :: Text,
    _quantity :: Maybe ItemQuantity
  }
  deriving (Generic, Show)

instance FromJSON OrderItem where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OrderItem where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example OrderItem where
  example =
    OrderItem
      { _id = idExample,
        _quantity = example
      }
