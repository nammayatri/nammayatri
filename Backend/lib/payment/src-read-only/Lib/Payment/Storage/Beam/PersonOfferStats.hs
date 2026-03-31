{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Beam.PersonOfferStats where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude

data PersonOfferStatsT f = PersonOfferStatsT
  { createdAt :: (B.C f Kernel.Prelude.UTCTime),
    id :: (B.C f Kernel.Prelude.Text),
    offerAppliedCount :: (B.C f Kernel.Prelude.Int),
    offerId :: (B.C f Kernel.Prelude.Text),
    personId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonOfferStatsT where
  data PrimaryKey PersonOfferStatsT f = PersonOfferStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonOfferStatsId . id

type PersonOfferStats = PersonOfferStatsT Identity

$(enableKVPG (''PersonOfferStatsT) [('id)] [])

$(mkTableInstancesGenericSchema (''PersonOfferStatsT) "person_offer_stats")
