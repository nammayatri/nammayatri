{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideBookingCancellationReason where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.CancellationReason (CancellationReasonCode, CancellationStage)
import Domain.Types.Ride (Ride)
import Domain.Types.RideBooking (RideBooking)

data RideBookingCancellationReason = RideBookingCancellationReason
  { id :: Id RideBookingCancellationReason,
    rideBookingId :: Id RideBooking,
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
  | ByOrganization
  | ByAllocator
  deriving (Show, Eq, Ord, Read, Generic)
