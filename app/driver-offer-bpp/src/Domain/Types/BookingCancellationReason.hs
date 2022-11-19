{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.BookingCancellationReason where

import Beckn.Types.Id
import qualified Domain.Types.Booking as DRB
import Domain.Types.CancellationReason (CancellationReasonCode)
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)

data BookingCancellationReason = BookingCancellationReason
  { id :: Id BookingCancellationReason,
    driverId :: Maybe (Id DP.Person),
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
  | ByApplication
  deriving (Show, Eq, Ord, Read, Generic)
