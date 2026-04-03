{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Payment.Storage.Beam.OfferStats where
import Kernel.Prelude
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Lib.Payment.Storage.Beam.BeamFlow ()
import qualified Kernel.Prelude
import qualified Lib.Payment.Domain.Types.OfferStats
import qualified Database.Beam as B



data OfferStatsT f
    = OfferStatsT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                   personId :: (B.C f Kernel.Prelude.Text),
                   entityType :: (B.C f (Kernel.Prelude.Maybe Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType)),
                   id :: (B.C f Kernel.Prelude.Text),
                   offerAppliedCount :: (B.C f Kernel.Prelude.Int),
                   offerId :: (B.C f Kernel.Prelude.Text),
                   updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table OfferStatsT
    where data PrimaryKey OfferStatsT f = OfferStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = OfferStatsId . id
type OfferStats = OfferStatsT Identity

$(enableKVPG (''OfferStatsT) [('id)] [])

$(mkTableInstancesGenericSchema (''OfferStatsT) "person_offer_stats")

