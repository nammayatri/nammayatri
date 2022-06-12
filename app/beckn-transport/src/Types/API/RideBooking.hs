module Types.API.RideBooking where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.FareBreakup (FareBreakupAPIEntity)
import Domain.Types.Ride (RideAPIEntity)
import Domain.Types.RideBooking (RideBooking, RideBookingStatus)
import EulerHS.Prelude hiding (id)
import Types.App (Driver)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    toLocation :: Maybe BookingLocationAPIEntity,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    fareBreakup :: [FareBreakupAPIEntity],
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
    pickupLoc :: BookingLocationAPIEntity,
    dropLoc :: Maybe BookingLocationAPIEntity,
    etaForPickupLoc :: Minutes,
    distanceToPickupLoc :: Meters,
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
