module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.Order (OrderItem)
import Beckn.Types.Core.Payment
import Beckn.Types.Mobility.Trip
import Data.Time.LocalTime
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Maybe Text,
    _created_at :: LocalTime,
    _updated_at :: LocalTime,
    _items :: [OrderItem],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment,
    -- Mobility specific
    _trip :: Maybe Trip
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions
