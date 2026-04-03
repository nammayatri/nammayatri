{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.OfferStats where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.OfferStats

data OfferStatsT f = OfferStatsT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    personId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType)),
    id :: (B.C f Kernel.Prelude.Text),
    offerAppliedCount :: (B.C f Kernel.Prelude.Int),
    offerId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table OfferStatsT where
  data PrimaryKey OfferStatsT f = OfferStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = OfferStatsId . id

type OfferStats = OfferStatsT Identity

$(enableKVPG (''OfferStatsT) [('id)] [])

$(mkTableInstancesGenericSchema (''OfferStatsT) "person_offer_stats")
