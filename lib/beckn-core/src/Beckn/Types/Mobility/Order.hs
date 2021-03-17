module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Option (Option)
import Beckn.Types.Core.Order (OrderItem)
import Beckn.Types.Core.Payment
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Example
import Data.Time
import EulerHS.Prelude

data Order = Order
  { _id :: Text,
    _state :: Maybe Text,
    _created_at :: UTCTime,
    _updated_at :: UTCTime,
    _items :: [OrderItem],
    _billing :: Maybe Billing,
    _payment :: Maybe Payment,
    -- Mobility specific
    _trip :: Maybe Trip,
    _cancellation_reason_id :: Maybe Text,
    _cancellation_reasons :: [Option],
    _cancellation_policy :: Maybe CancelPolicy
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Order where
  toJSON = genericToJSON stripAllLensPrefixOptions

data CancelPolicy = CancelPolicy
  { cancellation_fee :: MonetaryValue,
    refund :: MonetaryValue,
    cancel_by :: UTCTime,
    terms :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON CancelPolicy where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CancelPolicy where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example CancelPolicy where
  example =
    CancelPolicy
      { cancellation_fee = example,
        refund = example,
        cancel_by = example,
        terms = []
      }
