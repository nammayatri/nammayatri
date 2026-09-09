{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.OfferFrequencyStatsHistory where

import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Tools.Beam.UtilsTH

data OfferFrequencyStatsHistory = OfferFrequencyStatsHistory
  { appliedCount :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    entityId :: Kernel.Prelude.Text,
    entityType :: Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType,
    frequencyType :: Lib.Payment.Domain.Types.Offer.OfferFrequency,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfferFrequencyStatsHistory.OfferFrequencyStatsHistory,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    offerId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
    periodEnd :: Kernel.Prelude.UTCTime,
    periodStart :: Kernel.Prelude.UTCTime,
    totalCashbackAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDiscountAmount :: Kernel.Types.Common.HighPrecMoney,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)
