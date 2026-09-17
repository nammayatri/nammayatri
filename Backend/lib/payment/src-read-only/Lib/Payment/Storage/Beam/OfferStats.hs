{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.OfferStats where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.OfferStats

data OfferStatsT f = OfferStatsT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    personId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType),
    id :: B.C f Kernel.Prelude.Text,
    offerAppliedCount :: B.C f Kernel.Prelude.Int,
    offerId :: B.C f Kernel.Prelude.Text,
    totalCashbackAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalDiscountAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table OfferStatsT where
  data PrimaryKey OfferStatsT f = OfferStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfferStatsId . id

type OfferStats = OfferStatsT Identity

$(enableKVPG ''OfferStatsT ['id] [['personId]])

$(mkTableInstancesGenericSchema ''OfferStatsT "person_offer_stats")
