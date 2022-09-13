module Types.API.CustomerSupport where

import Data.OpenApi (ToSchema)
import Data.Time
import Domain.Types.Booking
import Domain.Types.Booking.BookingLocation
import qualified Domain.Types.Person as P
import EulerHS.Prelude hiding (id)

newtype OrderResp = OrderResp {order :: OrderDetails}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data OrderDetails = OrderDetails
  { id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    fromLocation :: BookingLocationAPIEntity,
    toLocation :: Maybe BookingLocationAPIEntity,
    travellerName :: Maybe Text,
    travellerPhone :: Maybe Text,
    rideBooking :: BookingAPIEntity
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OrderInfo = OrderInfo
  { person :: P.Person,
    bookings :: [Booking]
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
