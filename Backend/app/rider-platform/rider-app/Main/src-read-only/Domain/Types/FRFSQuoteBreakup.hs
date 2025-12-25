{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuoteBreakup where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteCategory
import qualified Domain.Types.FRFSQuoteCategorySpec
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuoteBreakup = FRFSQuoteBreakup
  { id :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteBreakup.FRFSQuoteBreakup,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    quoteCategoryId :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteCategory.FRFSQuoteCategory,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    tag :: Domain.Types.FRFSQuoteCategorySpec.FRFSCategoryTag,
    value :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
