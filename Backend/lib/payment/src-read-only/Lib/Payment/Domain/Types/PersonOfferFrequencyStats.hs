{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PersonOfferFrequencyStats where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Tools.Beam.UtilsTH

data PersonOfferFrequencyStats = PersonOfferFrequencyStats
  { appliedCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    entityId :: Kernel.Prelude.Text,
    entityType :: Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PersonOfferFrequencyStats.PersonOfferFrequencyStats,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    offerId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
    periodStart :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalCashbackAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDiscountAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
