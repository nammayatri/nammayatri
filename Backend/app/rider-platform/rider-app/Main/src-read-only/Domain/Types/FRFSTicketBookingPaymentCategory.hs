{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketBookingPaymentCategory where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketBookingPayment
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketBookingPaymentCategory = FRFSTicketBookingPaymentCategory
  { bppItemId :: Kernel.Prelude.Text,
    category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categoryMeta :: Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    frfsTicketBookingPaymentId :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPayment.FRFSTicketBookingPayment,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBookingPaymentCategory.FRFSTicketBookingPaymentCategory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    offeredPrice :: Kernel.Types.Common.Price,
    price :: Kernel.Types.Common.Price,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    selectedQuantity :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
