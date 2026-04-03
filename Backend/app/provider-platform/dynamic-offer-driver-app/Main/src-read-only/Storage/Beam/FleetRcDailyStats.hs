{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetRcDailyStats where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Data.Time.Calendar
import qualified Database.Beam as B



data FleetRcDailyStatsT f
    = FleetRcDailyStatsT {currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                          fleetOwnerId :: (B.C f Kernel.Prelude.Text),
                          merchantLocalDate :: (B.C f Data.Time.Calendar.Day),
                          rcId :: (B.C f Kernel.Prelude.Text),
                          rideDistance :: (B.C f Kernel.Prelude.Double),
                          rideDuration :: (B.C f Kernel.Types.Common.Seconds),
                          totalCompletedRides :: (B.C f Kernel.Prelude.Int),
                          totalEarnings :: (B.C f Kernel.Types.Common.HighPrecMoney),
                          merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                          merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                          createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetRcDailyStatsT
    where data PrimaryKey FleetRcDailyStatsT f = FleetRcDailyStatsId (B.C f Kernel.Prelude.Text) (B.C f Data.Time.Calendar.Day) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetRcDailyStatsId <$> fleetOwnerId <*> merchantLocalDate <*> rcId
type FleetRcDailyStats = FleetRcDailyStatsT Identity

$(enableKVPG (''FleetRcDailyStatsT) [('fleetOwnerId), ('merchantLocalDate), ('rcId)] [])

$(mkTableInstances (''FleetRcDailyStatsT) "fleet_rc_daily_stats")

