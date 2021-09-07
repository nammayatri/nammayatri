module Types.API.RideBooking where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.App (Driver)
import Types.Storage.ProductInstance (ProductInstance)
import Types.Storage.Ride (RideAPIEntity)
import Types.Storage.RideBooking (RideBookingStatus)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id ProductInstance,
    status :: RideBookingStatus,
    estimatedPrice :: Maybe Amount,
    toLocation :: SearchReqLocationAPIEntity,
    fromLocation :: SearchReqLocationAPIEntity,
    ride :: Maybe RideAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype RideBookingListRes = RideBookingListRes
  { list :: [RideBookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype GetRideInfoRes = GetRideInfoRes
  { rideRequest :: Maybe RideInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data RideInfo = RideInfo
  { bookingId :: Id ProductInstance,
    pickupLoc :: LatLong,
    dropLoc :: LatLong,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Double,
    notificationExpiryTime :: UTCTime,
    estimatedPrice :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show)

newtype SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { response :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type SetDriverAcceptanceRes = APISuccess

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON)

data DriverResponse = DriverResponse
  { driverId :: Id Driver,
    status :: NotificationStatus
  }
  deriving (Show, Generic, FromJSON, ToJSON)
