{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Ride
  ( module Dashboard.ProviderPlatform.Ride,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Dashboard.Common.Booking as Reexport (CancellationReasonCode (..))
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import Kernel.External.Maps.Types
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.JSON (constructorsWithLowerCase)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant hiding (Summary)

---------------------------------------------------------
-- ride list --------------------------------------------

type RideListAPI =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "bookingStatus" BookingStatus
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "customerPhoneNo" Text
    :> QueryParam "driverPhoneNo" Text
    :> QueryParam "fareDiff" Money
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] RideListRes

data RideListRes = RideListRes
  { totalItems :: Int, -- for backward compatibility
    summary :: Summary,
    rides :: [RideListItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideListItem = RideListItem
  { rideId :: Id Ride,
    rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    fareDiff :: Maybe Money,
    bookingStatus :: BookingStatus,
    rideCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | COMPLETED | CANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

---------------------------------------------------------
-- ride start -------------------------------------------

type RideStartAPI =
  Capture "rideId" (Id Ride)
    :> "start"
    :> ReqBody '[JSON] StartRideReq
    :> Post '[JSON] APISuccess

newtype StartRideReq = StartRideReq
  { point :: Maybe LatLong
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets StartRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- ride end ---------------------------------------------

type RideEndAPI =
  Capture "rideId" (Id Ride)
    :> "end"
    :> ReqBody '[JSON] EndRideReq
    :> Post '[JSON] APISuccess

data EndRideReq = EndRideReq
  { point :: Maybe LatLong,
    odometerEndReading :: Maybe Centesimal,
    endRideOtp :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets EndRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- multiple ride end ------------------------------

type MultipleRideEndAPI =
  "end"
    :> ReqBody '[JSON] MultipleRideEndReq
    :> Post '[JSON] MultipleRideEndResp

newtype MultipleRideEndReq = MultipleRideEndReq
  { rides :: [MultipleRideEndItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideEndItem = MultipleRideEndItem
  { rideId :: Id Ride,
    point :: Maybe LatLong -- FIXME not used for distance calculation, remove when possible
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideEndReq where
  hideSecrets = identity

type MultipleRideEndResp = MultipleRideSyncResp

validateMultipleRideEndReq :: Validate MultipleRideEndReq
validateMultipleRideEndReq MultipleRideEndReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

-- active ride id on the basis of vehcile number ---------------------------------------------

type CurrentActiveRideAPI =
  Capture "vehicleNumber" Text
    :> "currentActiveRide"
    :> Get '[JSON] (Id Ride)

-- ride cancel ------------------------------------------

type RideCancelAPI =
  Capture "rideId" (Id Ride)
    :> "cancel"
    :> ReqBody '[JSON] CancelRideReq
    :> Post '[JSON] APISuccess

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets CancelRideReq where
  hideSecrets = identity

---------------------------------------------------------
-- multiple ride cancel ---------------------------

type MultipleRideCancelAPI =
  "cancel"
    :> ReqBody '[JSON] MultipleRideCancelReq
    :> Post '[JSON] MultipleRideCancelResp

newtype MultipleRideCancelReq = MultipleRideCancelReq
  { rides :: [MultipleRideCancelItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideCancelItem = MultipleRideCancelItem
  { rideId :: Id Ride,
    reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideCancelReq where
  hideSecrets = identity

type MultipleRideCancelResp = MultipleRideSyncResp

validateMultipleRideCancelReq :: Validate MultipleRideCancelReq
validateMultipleRideCancelReq MultipleRideCancelReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- ride info --------------------------------------------

type RideInfoAPI =
  Capture "rideId" (Id Ride)
    :> "info"
    :> Get '[JSON] RideInfoRes

data RideInfoRes = RideInfoRes
  { rideId :: Id Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    rideOtp :: Text,
    customerPickupLocation :: LocationAPIEntity,
    customerDropLocation :: Maybe LocationAPIEntity,
    actualDropLocation :: Maybe LatLong,
    driverId :: Id Driver,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    driverStartLocation :: Maybe LatLong,
    driverCurrentLocation :: Maybe LatLong,
    rideBookingTime :: UTCTime,
    estimatedDriverArrivalTime :: Maybe UTCTime,
    actualDriverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe Meters,
    rideDistanceActual :: Meters,
    chargeableDistance :: Maybe Meters,
    maxEstimatedDistance :: Maybe Meters,
    estimatedRideDuration :: Maybe Minutes,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    estimatedFare :: Money,
    actualFare :: Maybe Money,
    driverOfferedFare :: Maybe Money,
    pickupDuration :: Maybe Minutes,
    rideDuration :: Maybe Minutes,
    bookingStatus :: BookingStatus,
    cancelledTime :: Maybe UTCTime,
    cancelledBy :: Maybe CancellationSource,
    cancellationReason :: Maybe CancellationReasonCode,
    driverInitiatedCallCount :: Int,
    bookingToRideStartDuration :: Maybe Minutes,
    distanceCalculationFailed :: Maybe Bool,
    vehicleVariant :: Maybe Variant,
    odometerStartReading :: Maybe Centesimal,
    odometerEndReading :: Maybe Centesimal
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAPIEntity = LocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

---------------------------------------------------------
-- ride sync ---------------------------------------------
type RideSyncAPI =
  Capture "rideId" (Id Ride)
    :> "sync"
    :> Post '[JSON] RideSyncRes

data RideSyncRes = RideSyncRes
  { newStatus :: RideStatus,
    message :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets RideSyncRes where
  hideSecrets = identity

data RideStatus
  = RIDE_NEW
  | RIDE_INPROGRESS
  | RIDE_COMPLETED
  | RIDE_CANCELLED
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

--------------------------- multipleRideSyncApi -----------------------------

type MultipleRideSyncAPI =
  "sync"
    :> ReqBody '[JSON] MultipleRideSyncReq
    :> Post '[JSON] MultipleRideSyncRes

newtype MultipleRideSyncReq = MultipleRideSyncReq
  { rideIds :: [Id Ride]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideSyncReq where
  hideSecrets = identity

newtype MultipleRideSyncRes = MultipleRideSyncRes
  { list :: [Either Text MultipleRideData]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data MultipleRideData = MultipleRideData
  { rideId :: Id Ride,
    newStatus :: RideStatus,
    message :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideSyncRes where
  hideSecrets = identity

---------------------------------------------------------
-- ride route -------------------------------------------

data Status
  = ON_RIDE
  | ON_PICKUP
  | IDLE
  deriving stock (Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type RideRouteAPI =
  Capture "rideId" (Id Ride)
    :> "route"
    :> Post '[JSON] RideRouteRes

data ActualRoute = ActualRoute
  { lat :: Double,
    lon :: Double,
    timestamp :: UTCTime,
    accuracy :: Maybe Double,
    rideStatus :: Maybe Status
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype RideRouteRes = RideRouteRes
  { actualRoute :: [ActualRoute]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverEdaKafka = DriverEdaKafka
  { driver_id :: String,
    rid :: Maybe String,
    ts :: String,
    acc :: Maybe String,
    rideStatus :: Maybe String,
    lat :: Maybe String,
    lon :: Maybe String,
    mid :: Maybe String,
    updated_at :: Maybe String,
    created_at :: Maybe String,
    on_ride :: Maybe String,
    active :: Maybe String,
    partition_date :: String,
    date :: String
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

instance HideSecrets RideRouteRes where
  hideSecrets = identity

---------------------------------------------------------
-- Booking with driver phone number and vehicle number ---------------------------------------

type BookingWithVehicleNumberAndPhoneAPI =
  "booking"
    :> "withVehicleNumberAndPhone"
    :> ReqBody '[JSON] BookingWithVehicleAndPhoneReq
    :> Post '[JSON] BookingWithVehicleAndPhoneRes

data BookingWithVehicleAndPhoneReq = BookingWithVehicleAndPhoneReq
  { vehicleNumber :: Text,
    phoneNumber :: Text,
    countryCode :: Text,
    endRideForDriver :: Bool,
    endRideForVehicle :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BookingWithVehicleAndPhoneReq where
  hideSecrets = identity

newtype BookingWithVehicleAndPhoneRes = BookingWithVehicleAndPhoneRes
  { driverId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BookingWithVehicleAndPhoneRes where
  hideSecrets = identity

-- ticket ride list --------------------------------------------

type TicketRideListAPI =
  "kapture"
    :> "list"
    :> QueryParam "rideShortId" (ShortId Ride)
    :> QueryParam "countryCode" Text
    :> QueryParam "phoneNumber" Text
    :> QueryParam "supportPhoneNumber" Text
    :> Get '[JSON] TicketRideListRes

newtype TicketRideListRes = TicketRideListRes
  { rides :: [RideInfo]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

data RideInfo = RideInfo
  { rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: BookingStatus,
    rideCreatedAt :: UTCTime,
    pickupLocationLat :: Maybe Double,
    pickupLocationLon :: Maybe Double,
    pickupLocationStreet :: Maybe Text,
    pickupLocationCity :: Maybe Text,
    pickupLocationState :: Maybe Text,
    pickupLocationCountry :: Maybe Text,
    pickupLocationBuilding :: Maybe Text,
    pickupLocationAreaCode :: Maybe Text,
    pickupLocationArea :: Maybe Text,
    dropLocationLat :: Maybe Double,
    dropLocationLon :: Maybe Double,
    dropLocationStreet :: Maybe Text,
    dropLocationCity :: Maybe Text,
    dropLocationState :: Maybe Text,
    dropLocationCountry :: Maybe Text,
    dropLocationBuilding :: Maybe Text,
    dropLocationAreaCode :: Maybe Text,
    dropLocationArea :: Maybe Text,
    fare :: Maybe Money,
    personId :: Id Driver,
    classification :: Ticket.Classification
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema)

instance FromJSON RideInfo where
  parseJSON = genericParseJSON constructorsWithLowerCase

instance ToJSON RideInfo where
  toJSON = genericToJSON constructorsWithLowerCase
