module Types.API.Ride where

import Beckn.Types.Core.Ack (Ack (..))
import Beckn.Types.Id
import Beckn.Types.Storage.ProductInstance (ProductInstance)
import Data.Time
import EulerHS.Prelude

data SetDriverAcceptanceReq = SetDriverAcceptanceReq
  { _productInstanceId :: Id ProductInstance,
    _response :: NotificationStatus
  }
  deriving (Show, Generic)

instance FromJSON SetDriverAcceptanceReq where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON SetDriverAcceptanceReq where
  toJSON = genericToJSON stripLensPrefixOptions

type SetDriverAcceptanceRes = Ack

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, ToJSON, FromJSON)

data DriverResponse = DriverResponse
  { _status :: NotificationStatus,
    _respondedAt :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON DriverResponse where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON DriverResponse where
  toJSON = genericToJSON stripLensPrefixOptions

newtype StartRideReq = StartRideReq
  { otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
