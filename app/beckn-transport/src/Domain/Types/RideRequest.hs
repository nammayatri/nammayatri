{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideRequest where

import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.Time (UTCTime)
import Domain.Types.Organization
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data RideRequestType = ALLOCATION | CANCELLATION | DRIVER_RESPONSE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable RideRequestType

data RideRequest = RideRequest
  { id :: Id RideRequest,
    rideBookingId :: Id DRB.RideBooking,
    shortOrgId :: ShortId Organization,
    createdAt :: UTCTime,
    _type :: RideRequestType,
    info :: Maybe Text
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, PrettyShow)
