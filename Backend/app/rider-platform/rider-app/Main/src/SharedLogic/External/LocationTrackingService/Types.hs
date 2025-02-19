{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Types where

import Data.Aeson
import Data.OpenApi hiding (Example, example, name, tags, url)
import EulerHS.Prelude
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data VehicleInfo = VehicleInfo
  { startTime :: Maybe UTCTime,
    scheduleRelationship :: Maybe Text,
    tripId :: Maybe Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    timestamp :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleTrackingOnRouteResp = VehicleTrackingOnRouteResp
  { vehicleNumber :: Text,
    vehicleInfo :: VehicleInfo
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleTrackingOnRouteReq = VehicleTrackingOnRouteReq
  { routeCode :: Maybe Text,
    tripCodes :: Maybe [Text]
  }
  deriving (Generic)

instance ToSchema VehicleTrackingOnRouteReq where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON VehicleTrackingOnRouteReq where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON VehicleTrackingOnRouteReq where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

newtype LocationTrackingeServiceConfig = LocationTrackingeServiceConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

type HasLocationService m r = (HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig])
