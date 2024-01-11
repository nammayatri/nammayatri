{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBookingPeopleCategory where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data TicketBookingPeopleCategory = TicketBookingPeopleCategory
  { id :: Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory,
    name :: Kernel.Prelude.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney,
    ticketBookingServiceCategoryId :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
