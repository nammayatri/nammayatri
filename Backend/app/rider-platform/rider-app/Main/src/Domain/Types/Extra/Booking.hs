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
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

activeBookingStatus :: [BookingStatus]
activeBookingStatus = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

terminalBookingStatus :: [BookingStatus]
terminalBookingStatus = [COMPLETED, CANCELLED, REALLOCATED]

activeScheduledBookingStatus :: [BookingStatus]
activeScheduledBookingStatus = [AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

deriving instance Ord PaymentStatus

$(mkBeamInstancesForEnum ''PaymentStatus)

$(mkHttpInstancesForEnum ''PaymentStatus)
