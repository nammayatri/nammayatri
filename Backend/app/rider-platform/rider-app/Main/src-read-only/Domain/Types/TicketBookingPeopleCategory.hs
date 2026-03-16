{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.TicketBookingPeopleCategory where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.ServicePeopleCategory
import qualified Domain.Types.TicketBookingServiceCategory
import qualified Tools.Payment
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data TicketBookingPeopleCategory
    = TicketBookingPeopleCategory {amountToRefund :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
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
                                   updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show)



