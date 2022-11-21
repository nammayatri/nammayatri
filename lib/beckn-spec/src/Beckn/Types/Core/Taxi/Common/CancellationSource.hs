{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Taxi.Common.CancellationSource where

import Beckn.Storage.Esqueleto
import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
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
        "ByMerchant" -> "CANCELLED_BY_MERCHANT"
        "ByAllocator" -> "CANCELLED_BY_ALLOCATOR"
        "ByApplication" -> "CANCELLED_BY_APPLICATION"
        _ -> error "CancellationReason parsing error"
    }

instance ToSchema CancellationSource where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions cancellationSourceJSONOptions
