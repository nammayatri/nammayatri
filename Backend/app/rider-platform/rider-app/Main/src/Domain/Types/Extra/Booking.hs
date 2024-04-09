{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.Booking
  ( module Domain.Types.Extra.Booking,
    PaymentStatus (..),
    BookingStatus (..),
  )
where

import BecknV2.OnDemand.Enums (PaymentStatus (..))
import Data.Aeson
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import Kernel.Types.Common
import Kernel.Types.Id
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
