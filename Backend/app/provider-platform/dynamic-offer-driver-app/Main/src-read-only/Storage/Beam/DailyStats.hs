{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DailyStats where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DailyStats
import Kernel.External.Encryption
import qualified Kernel.External.Payout.Juspay.Types.Payout
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DailyStatsT f = DailyStatsT
  { activatedValidRides :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bonusEarnings :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    cancellationCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    commissionCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverId :: B.C f Data.Text.Text,
    id :: B.C f Data.Text.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    merchantLocalDate :: B.C f Data.Time.Calendar.Day,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    numDriversOnboarded :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numFleetsOnboarded :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    numRides :: B.C f Kernel.Prelude.Int,
    onlineDuration :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    payoutOrderId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    payoutOrderStatus :: B.C f (Kernel.Prelude.Maybe Kernel.External.Payout.Juspay.Types.Payout.PayoutOrderStatus),
    payoutStatus :: B.C f (Kernel.Prelude.Maybe Domain.Types.DailyStats.PayoutStatus),
    referralCounts :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    referralEarnings :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tipAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    tollCharges :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalDistance :: B.C f Kernel.Types.Common.Meters,
    totalEarnings :: B.C f Kernel.Types.Common.Money,
    totalEarningsAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalRideTime :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DailyStatsT where
  data PrimaryKey DailyStatsT f = DailyStatsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = DailyStatsId . id

type DailyStats = DailyStatsT Identity

$(enableKVPG ''DailyStatsT ['id] [['driverId]])

$(mkTableInstances ''DailyStatsT "daily_stats")
