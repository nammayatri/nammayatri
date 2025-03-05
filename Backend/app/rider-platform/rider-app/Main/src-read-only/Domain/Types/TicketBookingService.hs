{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBookingService where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Payment

data TicketBookingService = TicketBookingService
  { amount :: Kernel.Types.Common.Price,
    bHourId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.BusinessHour.BusinessHour),
    bookedSeats :: Kernel.Prelude.Int,
    btype :: Domain.Types.BusinessHour.BusinessHourType,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    ticketBookingId :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    updatedAt :: Kernel.Prelude.UTCTime,
    vendorSplitDetails :: Kernel.Prelude.Maybe [Tools.Payment.VendorSplitDetails],
    verificationCount :: Kernel.Prelude.Int,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show)

data ServiceStatus = Pending | Failed | Confirmed | Verified | Cancelled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ServiceStatus)
