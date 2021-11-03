module Types.API.CustomerSupport where

import Beckn.Types.Amount
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id)
import Types.Common
import qualified Types.Storage.Person as P
import qualified Types.Storage.RideBooking as SRB
import Types.Storage.SearchReqLocation as L
import Types.Storage.SearchRequest as C

newtype OrderResp = OrderResp {order :: OrderDetails}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data OrderDetails = OrderDetails
  { id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    fromLocation :: Maybe L.SearchReqLocationAPIEntity,
    toLocation :: Maybe L.SearchReqLocationAPIEntity,
    vehicleVariant :: Maybe Text,
    travellerName :: Maybe Text,
    travellerPhone :: Maybe Text,
    trip :: Maybe TripDetails
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data TripDetails = TripDetails
  { id :: Text, -- Product Instance Id
    status :: SRB.RideBookingStatus,
    driver :: Maybe Driver, -- info -> driver
    vehicle :: Maybe Vehicle,
    provider :: Maybe Provider,
    price :: Maybe Amount,
    actualPrice :: Maybe Amount
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OrderInfo = OrderInfo
  { person :: P.Person,
    searchRequests :: [C.SearchRequest]
  }
  deriving (Generic)

data LoginReq = LoginReq
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { auth_token :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
