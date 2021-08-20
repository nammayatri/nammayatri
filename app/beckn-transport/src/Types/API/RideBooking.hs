module Types.API.RideBooking where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.App (Driver)
import Types.Storage.Ride (RideAPIEntity)
import Types.Storage.RideBooking (RideBookingStatus, RideBooking)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    estimatedPrice :: Maybe Amount,
    toLocation :: SearchReqLocationAPIEntity,
    fromLocation :: SearchReqLocationAPIEntity,
    ride :: Maybe RideAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideBookingListRes = RideBookingListRes
  { list :: [RideBookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

data RideInfo = RideInfo
  { bookingId :: Id RideBooking,
    pickupLoc :: LatLong,
    dropLoc :: LatLong,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Double,
    notificationExpiryTime :: UTCTime,
    estimatedPrice :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

newtype SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { response :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type SetDriverAcceptanceRes = APISuccess

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data DriverResponse = DriverResponse
  { driverId :: Id Driver,
    status :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
