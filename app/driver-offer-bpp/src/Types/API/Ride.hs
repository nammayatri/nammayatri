module Types.API.Ride where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
--import Domain.Types.CancellationReason (CancellationReasonCode)
import Beckn.Types.MapSearch
import Domain.Types.Ride
import Domain.Types.RideBooking.BookingLocation (BookingLocationAPIEntity)
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

{-
data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
-}

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
    estimatedBaseFare :: Amount,
    driverSelectedFare :: Amount,
    actualRideDistance :: HighPrecMeters,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
