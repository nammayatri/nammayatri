module Types.API.CustomerSupport where

import Data.OpenApi (ToSchema)
import Data.Time
import qualified Domain.Types.Person as P
import Domain.Types.SearchReqLocation as L
import Domain.Types.SearchRequest as C
import EulerHS.Prelude hiding (id)
import Types.API.RideBooking (RideBookingStatusRes)

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
    travellerName :: Maybe Text,
    travellerPhone :: Maybe Text,
    rideBooking :: Maybe RideBookingStatusRes
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
