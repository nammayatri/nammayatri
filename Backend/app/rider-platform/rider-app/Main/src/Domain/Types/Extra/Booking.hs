{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Domain.Types.Extra.Booking
  ( module Domain.Types.Extra.Booking,
    PaymentStatus (..),
    BookingStatus (..),
  )
where

import BecknV2.OnDemand.Enums (PaymentStatus (..))
import Data.Aeson
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

-- Extra code goes here --
activeBookingStatus :: [BookingStatus]
activeBookingStatus = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

activeScheduledBookingStatus :: [BookingStatus]
activeScheduledBookingStatus = [AWAITING_REASSIGNMENT, TRIP_ASSIGNED]

data BookingStatus
  = NEW
  | CONFIRMED
  | AWAITING_REASSIGNMENT
  | REALLOCATED
  | COMPLETED
  | CANCELLED
  | TRIP_ASSIGNED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

instance CH.ClickhouseValue BookingStatus

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)

deriving instance Ord PaymentStatus

$(mkBeamInstancesForEnum ''PaymentStatus)

$(mkHttpInstancesForEnum ''PaymentStatus)
