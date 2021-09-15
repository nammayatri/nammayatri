module Types.API.Ride where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Storage.CancellationReason (CancellationReasonCode)

newtype StartRideReq = StartRideReq
  { rideOtp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
