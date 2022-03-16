module Types.API.RideBooking where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.Ride (RideAPIEntity)
import Domain.Types.RideBooking (RideBooking, RideBookingStatus)
import Domain.Types.SearchReqLocation (SearchReqLocationAPIEntity)
import EulerHS.Prelude hiding (id)
import Types.App (Driver)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    toLocation :: Maybe SearchReqLocationAPIEntity,
    fromLocation :: SearchReqLocationAPIEntity,
    rideList :: [RideAPIEntity],
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
    pickupLoc :: SearchReqLocationAPIEntity,
    dropLoc :: Maybe SearchReqLocationAPIEntity,
    etaForPickupLoc :: Maybe Integer,
    distanceToPickupLoc :: Maybe Double,
    notificationExpiryTime :: UTCTime,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount
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
