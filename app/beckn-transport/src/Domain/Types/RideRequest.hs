{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideRequest where

import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.OpenApi hiding (info)
import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Organization
import EulerHS.Prelude hiding (id)
import Types.App (Driver)

data RideRequestType = ALLOCATION | CANCELLATION | DRIVER_RESPONSE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable RideRequestType

data RideRequest = RideRequest
  { id :: Id RideRequest,
    bookingId :: Id DRB.Booking,
    shortOrgId :: ShortId Organization,
    createdAt :: UTCTime,
    _type :: RideRequestType,
    info :: Maybe DriverResponse
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, PrettyShow)

data DriverResponse = DriverResponse
  { driverId :: Id Driver,
    status :: NotificationStatus
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON, ToSchema, PrettyShow)

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, Eq, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable NotificationStatus
