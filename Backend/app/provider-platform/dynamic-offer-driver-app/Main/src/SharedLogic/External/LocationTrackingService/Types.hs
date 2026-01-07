{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

data StartRideReq = StartRideReq
  { lat :: Double,
    lon :: Double,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    rideInfo :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, ToSchema)

data EndRideReq = EndRideReq
  { lat :: Double,
    lon :: Double,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    nextRideId :: Maybe (Id DRide.Ride),
    rideInfo :: Maybe RideInfo
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NearByReq = NearByReq
  { lat :: Double,
    lon :: Double,
    onRide :: Maybe Bool,
    vehicleType :: Maybe [VehicleVariant],
    radius :: Int,
    merchantId :: Id DM.Merchant,
    groupId :: Maybe Text, -- fleetOwnerId
    groupId2 :: Maybe Text -- operatorId
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data LocationTrackingeServiceConfig = LocationTrackingeServiceConfig
  { url :: BaseUrl,
    secondaryUrl :: Maybe BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data EndRideRes = EndRideRes
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverDetailsReq = DriverDetailsReq
  { driverId :: Id DP.Person,
    driverMode :: DriverMode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data RideDetailsReq = RideDetailsReq
  { rideId :: Id DRide.Ride,
    rideStatus :: DRide.RideStatus,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    isFutureRide :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    rideInfo :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, ToSchema, Show)

newtype DriversLocationReq = DriversLocationReq
  { driverIds :: [Id DP.Person]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverLocationReq = DriverLocationReq
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype DriverLocationResp = DriverLocationResp
  { loc :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type HasLocationService m r = (HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig])

data DriverBlockTillReq = DriverBlockTillReq
  { merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    blockTill :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BusRideInfo = BusRideInfo
  { routeCode :: Text,
    busNumber :: Text,
    source :: LatLong,
    destination :: LatLong,
    routeLongName :: Maybe Text,
    driverName :: Maybe Text,
    groupId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema)

data CarRideInfo = CarRideInfo
  { pickupLocation :: LatLong,
    minDistanceBetweenTwoPoints :: Maybe Int,
    rideStops :: Maybe [LatLong]
  }
  deriving (Show, Eq, Generic, ToSchema)

data PilotRideInfo = PilotRideInfo
  { destination :: LatLong,
    driverName :: Maybe Text,
    dutyType :: Maybe Text,
    endAddress :: Maybe Text,
    groupId :: Maybe Text,
    pilotNumber :: Text,
    scheduledTripTime :: Maybe UTCTime,
    source :: LatLong,
    startAddress :: Maybe Text,
    vipName :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema)

data RideInfo = Bus BusRideInfo | Car CarRideInfo | Pilot PilotRideInfo
  deriving (Show, Eq, Generic, ToSchema)

instance FromJSON RideInfo where
  parseJSON = withObject "RideInfo" $ \obj ->
    ( Bus
        <$> ( obj .: "bus" >>= \busObj ->
                BusRideInfo <$> busObj .: "routeCode"
                  <*> busObj .: "busNumber"
                  <*> busObj .: "source"
                  <*> busObj .: "destination"
                  <*> busObj .:? "routeLongName"
                  <*> busObj .:? "driverName"
                  <*> busObj .:? "groupId"
            )
    )
      <|> ( Car
              <$> ( obj .: "car" >>= \carObj ->
                      CarRideInfo <$> carObj .: "pickupLocation"
                        <*> carObj .:? "minDistanceBetweenTwoPoints"
                        <*> carObj .:? "rideStops"
                  )
          )
      <|> ( Pilot
              <$> ( obj .: "pilot" >>= \pilotObj ->
                      PilotRideInfo <$> pilotObj .: "destination"
                        <*> pilotObj .:? "driverName"
                        <*> pilotObj .:? "dutyType"
                        <*> pilotObj .:? "endAddress"
                        <*> pilotObj .:? "groupId"
                        <*> pilotObj .: "pilotNumber"
                        <*> pilotObj .:? "scheduledTripTime"
                        <*> pilotObj .: "source"
                        <*> pilotObj .:? "startAddress"
                        <*> pilotObj .:? "vipName"
                  )
          )

instance ToJSON RideInfo where
  toJSON = \case
    Bus (BusRideInfo routeCode busNumber source destination routeLongName driverName groupId) ->
      object
        [ "bus"
            .= object
              [ "routeCode" .= routeCode,
                "busNumber" .= busNumber,
                "source" .= source,
                "destination" .= destination,
                "routeLongName" .= routeLongName,
                "driverName" .= driverName,
                "groupId" .= groupId
              ]
        ]
    Car (CarRideInfo pickupLocation minDistanceBetweenTwoPoints rideStops) ->
      object
        [ "car"
            .= object
              [ "pickupLocation" .= pickupLocation,
                "minDistanceBetweenTwoPoints" .= minDistanceBetweenTwoPoints,
                "rideStops" .= rideStops
              ]
        ]
    Pilot (PilotRideInfo {..}) ->
      object
        [ "pilot"
            .= object
              [ "destination" .= destination,
                "driverName" .= driverName,
                "dutyType" .= dutyType,
                "endAddress" .= endAddress,
                "groupId" .= groupId,
                "pilotNumber" .= pilotNumber,
                "scheduledTripTime" .= scheduledTripTime,
                "source" .= source,
                "startAddress" .= startAddress,
                "vipName" .= vipName
              ]
        ]
