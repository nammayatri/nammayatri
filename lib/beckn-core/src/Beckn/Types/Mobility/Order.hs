module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Option (Option)
import Beckn.Types.Core.Order (OrderItem)
import Beckn.Types.Core.Payment
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude

data Order = Order
  { id :: Text,
    state :: Maybe Text,
    created_at :: UTCTime,
    updated_at :: UTCTime,
    items :: [OrderItem],
    billing :: Maybe Billing,
    payment :: Maybe Payment,
    -- Mobility specific
    trip :: Maybe Trip,
    cancellation_reason_id :: Maybe Text,
    cancellation_reasons :: [Option],
    cancellation_policy :: Maybe CancelPolicy
  }
  deriving (Generic, Show)

instance FromJSON Order where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Order where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data CancelPolicy = CancelPolicy
  { cancellation_fee :: MonetaryValue,
    refund :: MonetaryValue,
    cancel_by :: UTCTime,
    terms :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON CancelPolicy where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON CancelPolicy where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example CancelPolicy where
  example =
    CancelPolicy
      { cancellation_fee = example,
        refund = example,
        cancel_by = example,
        terms = []
      }
