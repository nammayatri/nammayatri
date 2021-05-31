module Beckn.Types.Core.Migration.Order where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Billing
import Beckn.Types.Core.Migration.Fulfillment (Fulfillment)
import Beckn.Types.Core.Migration.ItemQuantity
import Beckn.Types.Core.Migration.Payment
import Beckn.Types.Core.Migration.Quotation
import Data.Time
import EulerHS.Prelude hiding (State)

data Order = Order
  { _id :: Maybe Text,
    _state :: Maybe Text,
    _provider :: IdAndLocations,
    _items :: [OrderItem],
    _add_ons :: [IdObject],
    _offers :: [IdObject],
    _billing :: Billing,
    _fulfillment :: Fulfillment,
    _quote :: Quotation,
    _payment :: Payment,
    _created_at :: Maybe UTCTime,
    _updated_at :: Maybe UTCTime
  }
  deriving (Generic, Show)

data IdAndLocations = IdAndLocations
  { _id :: Text,
    _locations :: [IdObject]
  }
  deriving (Generic, Show)

data OrderItem = OrderItem
  { _id :: Text,
    _quantity :: ItemQuantity
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON OrderItem where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON OrderItem where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON IdAndLocations where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON IdAndLocations where
  toJSON = genericToJSON stripLensPrefixOptions
