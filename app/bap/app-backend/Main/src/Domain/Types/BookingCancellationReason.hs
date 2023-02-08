{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.BookingCancellationReason where

import Domain.Types.Booking (Booking)
import Domain.Types.CancellationReason (CancellationReasonCode, CancellationStage)
import Domain.Types.Ride (Ride)
import Kernel.Prelude
import Kernel.Types.Id

data BookingCancellationReason = BookingCancellationReason
  { bookingId :: Id Booking,
    rideId :: Maybe (Id Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    reasonStage :: Maybe CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving (Show, Eq, Ord, Read, Generic)
