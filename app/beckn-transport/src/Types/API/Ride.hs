module Types.API.Ride where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.Storage.CancellationReason
import Types.Storage.Ride
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)
import Types.Storage.Vehicle

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
