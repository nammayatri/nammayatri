{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Taxi.Common.CancellationSource where

import Beckn.Storage.DB.Utils (fromBackendRowEnum)
import Beckn.Storage.Esqueleto
import Data.Aeson
import Data.OpenApi
import Database.Beam.Backend
import Database.Beam.Postgres
import EulerHS.Prelude

data CancellationSource
  = ByUser
  | ByDriver
  | ByOrganization
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic)

derivePersistField "CancellationSource"

instance ToJSON CancellationSource where
  toJSON = genericToJSON cancellationSourceJSONOptions

instance FromJSON CancellationSource where
  parseJSON = genericParseJSON cancellationSourceJSONOptions

cancellationSourceJSONOptions :: Options
cancellationSourceJSONOptions =
  defaultOptions
    { constructorTagModifier = \case
        "ByUser" -> "CANCELLED_BY_USER"
        "ByDriver" -> "CANCELLED_BY_DRIVER"
        "ByOrganization" -> "CANCELLED_BY_ORGANIZATION"
        "ByAllocator" -> "CANCELLED_BY_ALLOCATOR"
        _ -> error "CancellationReason parsing error"
    }

instance ToSchema CancellationSource where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions cancellationSourceJSONOptions

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CancellationSource where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres CancellationSource where
  fromBackendRow = fromBackendRowEnum "CancellationSource"
