module Types.API.Ride where

import Beckn.Types.App
import Beckn.Types.Core.Ack (Ack (..))
import Data.Time
import EulerHS.Prelude

data SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { _productInstanceId :: ProductInstanceId,
    _response :: NotificationStatus
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type SetDriverAcceptanceRes = Ack

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON)

data DriverResponse = DriverResponse
  { _status :: NotificationStatus,
    _respondedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)
