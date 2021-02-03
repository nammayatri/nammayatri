module Types.API.Ride where

import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App
import EulerHS.Prelude

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