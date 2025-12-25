{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PersonStats where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PersonStats
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PersonStatsT f = PersonStatsT
  { backlogPayoutAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    backlogPayoutStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus),
    completedRides :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    driverCancelledRides :: B.C f Kernel.Prelude.Int,
    eveningPeakRides :: B.C f Kernel.Prelude.Int,
    isBackfilled :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    morningPeakRides :: B.C f Kernel.Prelude.Int,
    offPeakRides :: B.C f Kernel.Prelude.Int,
    personId :: B.C f Kernel.Prelude.Text,
    referralAmountPaid :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    referralCount :: B.C f Kernel.Prelude.Int,
    referralEarnings :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    referredByEarnings :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    referredByEarningsPayoutStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.PersonStats.PayoutStatus),
    ticketsBookedInEvent :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    userCancelledRides :: B.C f Kernel.Prelude.Int,
    validActivations :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    weekdayRides :: B.C f Kernel.Prelude.Int,
    weekendPeakRides :: B.C f Kernel.Prelude.Int,
    weekendRides :: B.C f Kernel.Prelude.Int
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonStatsT where
  data PrimaryKey PersonStatsT f = PersonStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PersonStatsId . personId

type PersonStats = PersonStatsT Identity

$(enableKVPG ''PersonStatsT ['personId] [])

$(mkTableInstances ''PersonStatsT "person_stats")
