module Types.API.Ride where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Beckn.Utils.JSON
import Data.Time
import EulerHS.Prelude
import Types.App

data SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { productInstanceId :: Id ProductInstance,
    response :: NotificationStatus
  }
  deriving (Show, Generic)

instance FromJSON SetDriverAcceptanceReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON SetDriverAcceptanceReq where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type SetDriverAcceptanceRes = APISuccess

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON)

data DriverResponse = DriverResponse
  { driverId :: Id Driver,
    status :: NotificationStatus,
    respondedAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON DriverResponse where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON DriverResponse where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype StartRideReq = StartRideReq
  { otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
