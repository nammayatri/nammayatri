{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBookingPeopleCategory where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ServicePeopleCategory
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Payment

data TicketBookingPeopleCategory = TicketBookingPeopleCategory
  { amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory,
    name :: Kernel.Prelude.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    numberOfUnitsCancelled :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    peopleCategoryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.ServicePeopleCategory.ServicePeopleCategory),
    pricePerUnit :: Kernel.Types.Common.Price,
    ticketBookingServiceCategoryId :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    vendorSplitDetails :: Kernel.Prelude.Maybe [Tools.Payment.VendorSplitDetails],
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
