{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBookingServiceCategory where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBookingService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Payment

data TicketBookingServiceCategory = TicketBookingServiceCategory
  { amount :: Kernel.Types.Common.Price,
    amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Int,
    btype :: Kernel.Prelude.Maybe Domain.Types.BusinessHour.BusinessHourType,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    eventCancelledBy :: Kernel.Prelude.Maybe Domain.Types.TicketBookingServiceCategory.CancelledBy,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    name :: Kernel.Prelude.Text,
    serviceCategoryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketBookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    vendorSplitDetails :: Kernel.Prelude.Maybe [Tools.Payment.VendorSplitDetails],
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data CancelledBy = User | Merchant deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancelledBy)
