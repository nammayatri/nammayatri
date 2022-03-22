module Types.API.Ride where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.CancellationReason (CancellationReasonCode)
import Domain.Types.Ride
import Domain.Types.SearchReqLocation
import Domain.Types.Vehicle

newtype StartRideReq = StartRideReq
  { rideOtp :: Text
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
    fromLocation :: SearchReqLocationAPIEntity,
    toLocation :: SearchReqLocationAPIEntity,
    discount :: Maybe Amount,
    driverName :: Maybe Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    computedFare :: Maybe Amount,
    computedTotalFare :: Maybe Amount,
    actualRideDistance :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
