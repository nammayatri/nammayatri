{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.BookingCancellationReason where

import Beckn.Types.Id
import Domain.Types.CancellationReason (CancellationReasonCode)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data BookingCancellationReason = BookingCancellationReason
  { id :: Id BookingCancellationReason,
    driverId :: Maybe (Id DP.Person),
    bookingId :: Id DRB.RideBooking,
    rideId :: Maybe (Id DRide.Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic)

data CancellationSource
  = ByUser
  | ByDriver
  | ByOrganization
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic)
