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
import Data.Time
import qualified Domain.Types.VehicleVariant as VV
import EulerHS.Prelude
import Kernel.External.Maps.HasCoordinates
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data UpcomingStopStatus = Reached | Upcoming deriving (Generic, FromJSON, ToJSON, ToSchema, Eq, Show)

data Stop = Stop
  { name :: Text,
    coordinate :: LatLong,
    stopCode :: Text,
    stopIdx :: Int,
    distanceToUpcomingIntermediateStop :: Int,
    durationToUpcomingIntermediateStop :: Int
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data UpcomingStop = UpcomingStop
  { stop :: Stop,
    eta :: UTCTime,
    status :: UpcomingStopStatus,
    delta :: Maybe Double
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show)

data VehicleInfo = VehicleInfo
  { startTime :: Maybe UTCTime,
    scheduleRelationship :: Maybe Text,
    tripId :: Maybe Text,
    latitude :: Double,
    longitude :: Double,
    speed :: Maybe Double,
    timestamp :: Maybe Text,
    upcomingStops :: Maybe [UpcomingStop]
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

data LocationTrackingeServiceConfig = LocationTrackingeServiceConfig
  { url :: BaseUrl,
    secondaryUrl :: Maybe BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

type HasLocationService m r = (HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig])

data NearByDriverReq = NearByDriverReq
  { lat :: Double,
    lon :: Double,
    onRide :: Maybe Bool,
    vehicleType :: Maybe [VV.VehicleVariant],
    radius :: Meters,
    merchantId :: Text,
    groupId :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data Driver = Driver

data NearByDriverRes = NearByDriverRes
  { driverId :: Id Driver,
    lat :: Double,
    lon :: Double,
    coordinatesCalculatedAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    bear :: Maybe Double,
    vehicleType :: VV.VehicleVariant,
    rideDetails :: Maybe RideDetails
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data RideDetails = RideDetails
  { rideId :: Text,
    rideInfo :: Maybe RideInfo
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data BusRideInfo = BusRideInfo
  { routeCode :: Text,
    busNumber :: Text,
    destination :: LatLong,
    routeLongName :: Maybe Text,
    driverName :: Maybe Text,
    groupId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema)

data CarRideInfo = CarRideInfo
  { pickupLocation :: LatLong
  }
  deriving (Show, Eq, Generic, ToSchema)

data RideInfo = Bus BusRideInfo | Car CarRideInfo
  deriving (Show, Eq, Generic, ToSchema)

instance FromJSON RideInfo where
  parseJSON = withObject "RideInfo" $ \obj ->
    ( Bus
        <$> ( obj .: "bus" >>= \busObj ->
                BusRideInfo <$> busObj .: "routeCode"
                  <*> busObj .: "busNumber"
                  <*> busObj .: "destination"
                  <*> busObj .:? "routeLongName"
                  <*> busObj .:? "driverName"
                  <*> busObj .:? "groupId"
            )
    )
      <|> ( Car
              <$> ( obj .: "car" >>= \carObj ->
                      CarRideInfo <$> carObj .: "pickupLocation"
                  )
          )

instance ToJSON RideInfo where
  toJSON = \case
    Bus (BusRideInfo routeCode busNumber destination routeLongName driverName groupId) ->
      object
        [ "bus"
            .= object
              [ "routeCode" .= routeCode,
                "busNumber" .= busNumber,
                "destination" .= destination,
                "routeLongName" .= routeLongName,
                "driverName" .= driverName,
                "groupId" .= groupId
              ]
        ]
    Car (CarRideInfo pickupLocation) ->
      object
        [ "car"
            .= object
              [ "pickupLocation" .= pickupLocation
              ]
        ]
