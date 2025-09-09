{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuoteCategory where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuoteCategory = FRFSQuoteCategory
  { bppItemId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory,
    maxTicketAllowed :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    offeredPrice :: Kernel.Types.Common.Price,
    price :: Kernel.Types.Common.Price,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    ticketCategoryMetadataConfig :: Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
