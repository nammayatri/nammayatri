{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonStats where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PersonStatsT f = PersonStatsT
  { backfilledFromCkhTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    completedRides :: (B.C f Kernel.Prelude.Int),
    createdAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    driverCancelledRides :: (B.C f Kernel.Prelude.Int),
    eveningPeakRides :: (B.C f Kernel.Prelude.Int),
    isBackfilled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    morningPeakRides :: (B.C f Kernel.Prelude.Int),
    offPeakRides :: (B.C f Kernel.Prelude.Int),
    personId :: (B.C f Kernel.Prelude.Text),
    referralCount :: (B.C f Kernel.Prelude.Int),
    ticketsBookedInEvent :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    userCancelledRides :: (B.C f Kernel.Prelude.Int),
    weekdayRides :: (B.C f Kernel.Prelude.Int),
    weekendPeakRides :: (B.C f Kernel.Prelude.Int),
    weekendRides :: (B.C f Kernel.Prelude.Int)
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonStatsT where
  data PrimaryKey PersonStatsT f = PersonStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonStatsId . personId

type PersonStats = PersonStatsT Identity

$(enableKVPG (''PersonStatsT) [('personId)] [])

$(mkTableInstances (''PersonStatsT) "person_stats")
