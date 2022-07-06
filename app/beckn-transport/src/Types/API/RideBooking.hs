module Types.API.RideBooking where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Time (UTCTime)
import Domain.Types.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.FareBreakup (FareBreakupAPIEntity)
import Domain.Types.Ride (RideAPIEntity)
import Domain.Types.RideBooking (RideBooking, RideBookingStatus)
import EulerHS.Prelude hiding (id)
import qualified Tools.JSON as J
import qualified Tools.Schema as S
import Types.App (Driver)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: RideBookingAPIDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data RideBookingAPIDetails = OneWayAPIDetails OneWayRideBookingAPIDetails | RentalAPIDetails RentalRideBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON RideBookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON RideBookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema RideBookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayRideBookingAPIDetails = OneWayRideBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalRideBookingAPIDetails = RentalRideBookingAPIDetails
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

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
