module Beckn.Types.Mobility.Order where

import Beckn.Types.Core.Billing
import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.Option (Option)
import Beckn.Types.Core.Order (OrderItem)
import Beckn.Types.Core.Payment
import Beckn.Types.Mobility.Trip
import Beckn.Utils.Example
import Data.Aeson.Types
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time
import Database.Beam (FromBackendRow)
import Database.Beam.Backend (FromBackendRow (fromBackendRow), HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id, state)

data Order = Order
  { id :: Text,
    state :: Maybe OrderState,
    created_at :: UTCTime,
    updated_at :: UTCTime,
    items :: [OrderItem],
    billing :: Maybe Billing,
    payment :: Maybe Payment,
    -- Mobility specific
    trip :: Maybe Trip,
    cancellation_reason_id :: Maybe CancellationSource,
    cancellation_reasons :: [Option],
    cancellation_policy :: Maybe CancelPolicy
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data CancelPolicy = CancelPolicy
  { cancellation_fee :: MonetaryValue,
    refund :: MonetaryValue,
    cancel_by :: UTCTime,
    terms :: [Text]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OrderState = CONFIRMED | TRIP_ASSIGNED | INPROGRESS | COMPLETED | CANCELLED
  deriving (Generic, Eq, FromJSON, ToJSON, Show, ToSchema)

instance Example CancelPolicy where
  example =
    CancelPolicy
      { cancellation_fee = example,
        refund = example,
        cancel_by = example,
        terms = []
      }

data CancellationSource
  = ByUser
  | ByDriver
  | ByOrganization
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic, ToSchema)

instance ToJSON CancellationSource where
  toJSON = \case
    ByUser -> "CANCELLED_BY_USER"
    ByDriver -> "CANCELLED_BY_DRIVER"
    ByOrganization -> "CANCELLED_BY_ORGANIZATION"
    ByAllocator -> "CANCELLED_BY_ALLOCATOR"

instance FromJSON CancellationSource where
  parseJSON =
    withText
      "CancellationReason"
      ( \case
          "CANCELLED_BY_USER" -> return ByUser
          "CANCELLED_BY_DRIVER" -> return ByDriver
          "CANCELLED_BY_ORGANIZATION" -> return ByOrganization
          "CANCELLED_BY_ALLOCATOR" -> return ByAllocator
          _ -> fail "CancellationReason parsing error"
      )

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CancellationSource where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres CancellationSource where
  fromBackendRow = read . T.unpack <$> fromBackendRow
