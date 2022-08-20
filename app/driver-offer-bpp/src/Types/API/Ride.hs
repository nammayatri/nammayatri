module Types.API.Ride where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Domain.Types.Booking.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.CancellationReason (CancellationReasonCode)
import Domain.Types.Ride
import Domain.Types.Vehicle.Variant

data StartRideReq = StartRideReq
  { rideOtp :: Text,
    point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype EndRideReq = EndRideReq
  { point :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverRideRes = DriverRideRes
  { id :: Id Ride,
    shortRideId :: ShortId Ride,
    status :: RideStatus,
    fromLocation :: BookingLocationAPIEntity,
    toLocation :: BookingLocationAPIEntity,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Amount,
    estimatedBaseFare :: Amount,
    driverSelectedFare :: Amount,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
