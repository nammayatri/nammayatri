module Types.API.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import Types.Storage.Ride (RideAPIEntity)
import Types.Storage.RideBooking (RideBooking, RideBookingStatus)
import Types.Storage.SearchReqLocation (SearchReqLocationAPIEntity)

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    agencyName :: Text,
    agencyNumber :: Maybe Text,
    estimatedPrice :: Maybe Amount,
    toLocation :: SearchReqLocationAPIEntity,
    fromLocation :: SearchReqLocationAPIEntity,
    ride :: Maybe RideAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideBookingListRes = RideBookingListRes
  { list :: [RideBookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
