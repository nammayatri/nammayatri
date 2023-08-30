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

module Dashboard.RiderPlatform.Ride
  ( module Dashboard.RiderPlatform.Ride,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import qualified Dashboard.Common as DP
import Dashboard.Common.Ride as Reexport
import Data.Aeson
import Kernel.External.Maps
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Centesimal
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Kernel.Utils.Validation
import Servant hiding (Summary)

---------------------------------------------------------
-- share ride info--------------------------------------

type ShareRideInfoAPI =
  Capture "rideId" (Id DP.Ride)
    :> "info"
    :> Get '[JSON] ShareRideInfoRes

type RideInfoAPI =
  "rideinfo"
    :> Capture "rideId" (Id DP.Ride)
    :> Get '[JSON] RideInfoRes

data ShareRideInfoRes = ShareRideInfoRes
  { id :: Id Ride,
    bookingId :: Id Booking,
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    trackingUrl :: Maybe BaseUrl,
    estimatedDistance :: Maybe HighPrecMeters,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    fromLocation :: BookingLocation,
    toLocation :: Maybe BookingLocation
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data RideInfoRes = RideInfoRes
  { rideId :: Id Ride,
    bookingId :: Id Booking,
    rideStatus :: RideStatus,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    rideOtp :: Text,
    customerPickupLocation :: BookingLocation,
    customerDropLocation :: Maybe BookingLocation,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    driverRegisteredAt :: UTCTime,
    vehicleNo :: Text,
    vehicleModel :: Text,
    rideBookingTime :: UTCTime,
    actualDriverArrivalTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    rideDistanceEstimated :: Maybe HighPrecMeters,
    rideDistanceActual :: Maybe HighPrecMeters,
    chargeableDistance :: Maybe HighPrecMeters,
    estimatedFare :: Money,
    actualFare :: Maybe Money,
    estimatedRideDuration :: Maybe Seconds,
    rideDuration :: Maybe Seconds,
    cancelledTime :: Maybe UTCTime,
    cancelledBy :: Maybe CancellationSource
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data BookingLocation = BookingLocation
  { id :: Id BookingLocation,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToSchema, FromJSON, ToJSON)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)

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
  { rideShortId :: ShortId Ride,
    rideCreatedAt :: UTCTime,
    rideId :: Id Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Text,
    driverPhoneNo :: Text,
    vehicleNo :: Text,
    bookingStatus :: BookingStatus
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BookingStatus = UPCOMING | UPCOMING_6HRS | ONGOING | ONGOING_6HRS | RCOMPLETED | RCANCELLED
  deriving stock (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

derivePersistField "BookingStatus"

$(mkHttpInstancesForEnum ''BookingStatus)

---------------------------------------------------------
-- Trip Route--------------------------------------

type TripRouteAPI =
  "trip"
    :> "route"
    :> Capture "rideId" (Id DP.Ride)
    :> MandatoryQueryParam "lat" Double
    :> MandatoryQueryParam "lon" Double
    :> Get '[JSON] Maps.GetRoutesResp

---------------------------------------------------------
-- multiple ride sync -----------------------------

type MultipleRideSyncAPI =
  "sync"
    :> ReqBody '[JSON] MultipleRideSyncReq
    :> Post '[JSON] MultipleRideSyncResp

newtype MultipleRideSyncReq = MultipleRideSyncReq
  { rides :: [MultipleRideItem]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype MultipleRideItem = MultipleRideItem
  { rideId :: Id Ride
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideSyncReq where
  hideSecrets = identity

validateMultipleRideSyncReq :: Validate MultipleRideSyncReq
validateMultipleRideSyncReq MultipleRideSyncReq {..} = do
  validateField "rides" rides $ UniqueField @"rideId"

---------------------------------------------------------
-- Ticket Ride List--------------------------------------

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
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets TicketRideListRes where
  hideSecrets = identity

data RideInfo = RideInfo
  { rideShortId :: ShortId Ride,
    customerName :: Maybe Text,
    customerPhoneNo :: Maybe Text,
    driverName :: Text,
    driverPhoneNo :: Maybe Text,
    vehicleNo :: Text,
    status :: BookingStatus,
    rideCreatedAt :: UTCTime,
    pickupLocation :: BookingLocation,
    dropLocation :: Maybe BookingLocation,
    fare :: Maybe Money,
    personId :: Id Customer,
    classification :: Ticket.Classification
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
