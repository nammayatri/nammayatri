{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PersonOfferFrequencyStats where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.OfferStats

data PersonOfferFrequencyStatsT f = PersonOfferFrequencyStatsT
  { appliedCount :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f Kernel.Types.Common.Currency,
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offerId :: B.C f Kernel.Prelude.Text,
    periodStart :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    totalCashbackAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    totalDiscountAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonOfferFrequencyStatsT where
  data PrimaryKey PersonOfferFrequencyStatsT f = PersonOfferFrequencyStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonOfferFrequencyStatsId . id

type PersonOfferFrequencyStats = PersonOfferFrequencyStatsT Identity

$(enableKVPG ''PersonOfferFrequencyStatsT ['id] [['entityId]])

$(mkTableInstancesGenericSchema ''PersonOfferFrequencyStatsT "person_offer_frequency_stats")
