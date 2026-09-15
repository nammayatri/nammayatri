{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.Extra.Booking
  ( module Domain.Types.Extra.Booking,
    PaymentStatus (..),
  )
where

import BecknV2.OnDemand.Enums (PaymentStatus (..))
import Data.Aeson
import Domain.Types.BookingStatus
import Domain.Types.LocationAddress (LocationAddress (..))
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

--The rider's original pickup and drop, snapshotted at confirm time so rebooking preserves the intended trip (A → B) rather than the adjusted walk-and-save endpoints (A' → B').
data ParentSearchRequestLocationInfo = ParentSearchRequestLocationInfo
  { sourceLat :: Double,
    sourceLon :: Double,
    sourceAddress :: LocationAddress,
    destLat :: Double,
    destLon :: Double,
    destAddress :: LocationAddress
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

activeBookingStatus :: [BookingStatus]
activeBookingStatus = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

terminalBookingStatus :: [BookingStatus]
terminalBookingStatus = [COMPLETED, CANCELLED, REALLOCATED]

activeScheduledBookingStatus :: [BookingStatus]
activeScheduledBookingStatus = [AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

deriving instance Ord PaymentStatus

$(mkBeamInstancesForEnum ''PaymentStatus)

$(mkHttpInstancesForEnum ''PaymentStatus)
