{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBookingServiceCategory where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBookingService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data TicketBookingServiceCategory = TicketBookingServiceCategory
  { id :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    ticketBookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService,
    name :: Kernel.Prelude.Text,
    bookedSeats :: Kernel.Prelude.Int,
    amount :: Kernel.Types.Common.Price,
    eventCancelledBy :: Kernel.Prelude.Maybe Domain.Types.TicketBookingServiceCategory.CancelledBy,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    serviceCategoryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    btype :: Kernel.Prelude.Maybe Domain.Types.BusinessHour.BusinessHourType,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data CancelledBy = User | Merchant deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancelledBy)
