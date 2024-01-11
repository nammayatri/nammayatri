{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBookingService where

import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingService = TicketBookingService
  { amount :: Kernel.Types.Common.HighPrecMoney,
    btype :: Domain.Types.BusinessHour.BusinessHourType,
    createdAt :: Kernel.Prelude.UTCTime,
    expiryDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBookingService.TicketBookingService,
    status :: Domain.Types.TicketBookingService.ServiceStatus,
    ticketBookingId :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    ticketServiceId :: Kernel.Types.Id.Id Domain.Types.TicketService.TicketService,
    updatedAt :: Kernel.Prelude.UTCTime,
    verificationCount :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ServiceStatus = Pending | Failed | Confirmed | Verified
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''ServiceStatus)
