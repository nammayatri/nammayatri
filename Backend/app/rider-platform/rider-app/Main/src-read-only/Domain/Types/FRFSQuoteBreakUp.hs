{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSQuoteBreakUp where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSQuoteBreakUp = FRFSQuoteBreakUp
  { amount :: Kernel.Types.Common.Price,
    description :: Domain.Types.FRFSQuoteBreakUp.QuoteBreakupDescription,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSQuoteBreakUp.FRFSQuoteBreakUp,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Show)

data QuoteBreakupDescription = EstimatedChildFare | EstimatedAdultFare | PreBookingAdultFare | PreBookingChildFare deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''QuoteBreakupDescription))
