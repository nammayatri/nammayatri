module Beckn.Types.Core.Taxi.Common.CancellationSource where

import Beckn.Storage.DB.Utils (fromBackendRowEnum)
import Data.Aeson
import Data.OpenApi (ToSchema)
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude

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
  fromBackendRow = fromBackendRowEnum "CancellationSource"
