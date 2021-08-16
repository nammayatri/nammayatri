module Types.API.Ride where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import EulerHS.Prelude
import Types.App
import Types.Storage.CancellationReason (CancellationReasonCode)
import Types.Storage.ProductInstance (ProductInstance)

data SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { productInstanceId :: Id ProductInstance,
    response :: NotificationStatus
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

newtype StartRideReq = StartRideReq
  { otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
