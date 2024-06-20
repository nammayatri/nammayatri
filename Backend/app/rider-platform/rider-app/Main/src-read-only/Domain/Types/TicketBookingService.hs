{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBookingService where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TicketBookingService = TicketBookingService
  { id :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService,
    ticketBookingId :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    amount :: Kernel.Types.Common.Price,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    verificationCount :: Kernel.Prelude.Int,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    btype :: Domain.Types.BusinessHour.BusinessHourType,
    bHourId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour),
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    bookedSeats :: Kernel.Prelude.Int,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show)

data ServiceStatus = Pending | Failed | Confirmed | Verified | Cancelled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ServiceStatus)
