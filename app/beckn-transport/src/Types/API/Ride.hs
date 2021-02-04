module Types.API.Ride where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App
import Data.Time
import EulerHS.Prelude
import Types.App

data SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { _productInstanceId :: ProductInstanceId,
    _response :: NotificationStatus
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type SetDriverAcceptanceRes = APIResult.APIResult

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON)

data DriverResponse = DriverResponse
  { _driverId :: DriverId,
    _productInstanceId :: ProductInstanceId,
    _status :: NotificationStatus,
    _respondedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)