{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.OfferFrequencyStatsHistory where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Offer
import qualified Lib.Payment.Domain.Types.OfferStats

data OfferFrequencyStatsHistoryT f = OfferFrequencyStatsHistoryT
  { appliedCount :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f Kernel.Types.Common.Currency,
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType,
    frequencyType :: B.C f Lib.Payment.Domain.Types.Offer.OfferFrequency,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offerId :: B.C f Kernel.Prelude.Text,
    periodEnd :: B.C f Kernel.Prelude.UTCTime,
    periodStart :: B.C f Kernel.Prelude.UTCTime,
    totalCashbackAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    totalDiscountAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OfferFrequencyStatsHistoryT where
  data PrimaryKey OfferFrequencyStatsHistoryT f = OfferFrequencyStatsHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfferFrequencyStatsHistoryId . id

type OfferFrequencyStatsHistory = OfferFrequencyStatsHistoryT Identity

$(enableKVPG ''OfferFrequencyStatsHistoryT ['id] [['entityId]])

$(mkTableInstancesGenericSchema ''OfferFrequencyStatsHistoryT "offer_frequency_stats_history")
