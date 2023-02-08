{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.BookingCancellationReason where

import qualified Domain.Types.Booking as DRB
import Domain.Types.CancellationReason (CancellationReasonCode)
import Domain.Types.Person (Person)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id

data BookingCancellationReason = BookingCancellationReason
  { driverId :: Maybe (Id Person),
    bookingId :: Id DRB.Booking,
    rideId :: Maybe (Id DRide.Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic)
