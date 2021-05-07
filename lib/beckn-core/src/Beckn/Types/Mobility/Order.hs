module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Option (Option)
import Beckn.Types.Core.Order (OrderItem)
import Beckn.Types.Core.Payment
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson.Types
import Data.Time
import EulerHS.Prelude hiding (id, state)

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
    cancellation_reason_id :: Maybe CancellationReason,
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

data CancellationReason
  = ByUser
  | ByDriver
  | ByOrganization
  | AllocationTimeExpired
  | NoDriversInRange
  deriving (Show, Eq, Ord, Read, Generic)

instance ToJSON CancellationReason where
  toJSON = \case
    ByUser -> "CANCELLED_BY_USER"
    ByDriver -> "CANCELLED_BY_DRIVER"
    ByOrganization -> "CANCELLED_BY_ORGANIZATION"
    AllocationTimeExpired -> "ALLOCATION_TIME_EXPIRED"
    NoDriversInRange -> "NO_DRIVERS_IN_RANGE"

instance FromJSON CancellationReason where
  parseJSON =
    withText
      "CancellationReason"
      ( \case
          "CANCELLED_BY_USER" -> return ByUser
          "CANCELLED_BY_DRIVER" -> return ByDriver
          "CANCELLED_BY_ORGANIZATION" -> return ByOrganization
          "ALLOCATION_TIME_EXPIRED" -> return AllocationTimeExpired
          "NO_DRIVERS_IN_RANGE" -> return NoDriversInRange
          _ -> fail "CancellationReason parsing error"
      )
