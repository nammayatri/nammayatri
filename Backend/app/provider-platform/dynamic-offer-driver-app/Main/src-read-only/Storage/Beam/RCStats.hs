{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.RCStats where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Database.Beam as B



data RCStatsT f
    = RCStatsT {rcId :: (B.C f Kernel.Prelude.Text),
                totalRides :: (B.C f Kernel.Prelude.Int),
                updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                createdAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table RCStatsT
    where data PrimaryKey RCStatsT f = RCStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = RCStatsId . rcId
type RCStats = RCStatsT Identity

$(enableKVPG (''RCStatsT) [('rcId)] [])

$(mkTableInstances (''RCStatsT) "rc_stats")

