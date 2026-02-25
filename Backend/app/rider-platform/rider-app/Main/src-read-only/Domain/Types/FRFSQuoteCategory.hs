{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuoteCategory where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Seat
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuoteCategory = FRFSQuoteCategory
  { bppItemId :: Kernel.Prelude.Text,
    category :: Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType,
    categoryMeta :: Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategory.QuoteCategoryMetadata,
    finalPrice :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    offeredPrice :: Kernel.Types.Common.Price,
    price :: Kernel.Types.Common.Price,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    seatIds :: Kernel.Prelude.Maybe [Kernel.Types.Id.Id Domain.Types.Seat.Seat],
    seatLabels :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    selectedQuantity :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteCategoryMetadata = QuoteCategoryMetadata {code :: Kernel.Prelude.Text, description :: Kernel.Prelude.Text, title :: Kernel.Prelude.Text, tnc :: Kernel.Prelude.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
