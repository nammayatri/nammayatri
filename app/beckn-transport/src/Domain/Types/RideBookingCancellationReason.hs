{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideBookingCancellationReason
  ( module Reexport,
    module Domain.Types.RideBookingCancellationReason,
  )
where

import Beckn.Types.Core.Taxi.Common.CancellationSource as Reexport (CancellationSource (..))
import Beckn.Types.Id
import Domain.Types.CancellationReason (CancellationReasonCode)
import Domain.Types.Person (Person)
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)

data RideBookingCancellationReason = RideBookingCancellationReason
  { id :: Id RideBookingCancellationReason,
    driverId :: Maybe (Id Person),
    rideBookingId :: Id DRB.RideBooking,
    rideId :: Maybe (Id DRide.Ride),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    additionalInfo :: Maybe Text
  }
  deriving (Generic)
