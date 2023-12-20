{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Taxi.Common.CancellationSource where -- rename file to Cancellation?

import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.JSON
import Kernel.Utils.Schema

newtype Cancellation = Cancellation
  { reason :: CancellationSource
  }
  deriving (Show, Generic)

instance ToJSON Cancellation where
  toJSON = genericToJSON removeNullFields

instance FromJSON Cancellation where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema Cancellation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Ord, Read, Generic)

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

derivePersistField "CancellationSource"
