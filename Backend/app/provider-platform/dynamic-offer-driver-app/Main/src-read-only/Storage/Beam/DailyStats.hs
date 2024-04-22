{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DailyStats where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DailyStatsT f = DailyStatsT
  { currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    driverId :: B.C f Data.Text.Text,
    id :: B.C f Data.Text.Text,
    merchantLocalDate :: B.C f Data.Time.Calendar.Day,
    numRides :: B.C f Kernel.Prelude.Int,
    totalDistance :: B.C f Kernel.Types.Common.Meters,
    totalEarnings :: B.C f Kernel.Types.Common.Money,
    totalEarningsAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
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
