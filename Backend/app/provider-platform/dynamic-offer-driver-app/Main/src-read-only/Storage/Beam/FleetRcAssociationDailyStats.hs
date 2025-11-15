{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetRcAssociationDailyStats where

import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FleetRcAssociationDailyStatsT f = FleetRcAssociationDailyStatsT
  { currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    fleetOwnerId :: (B.C f Data.Text.Text),
    merchantLocalDate :: (B.C f Data.Time.Calendar.Day),
    rcId :: (B.C f Data.Text.Text),
    rideDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    rideDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    totalCompletedRides :: (B.C f Kernel.Prelude.Int),
    totalEarnings :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetRcAssociationDailyStatsT where
  data PrimaryKey FleetRcAssociationDailyStatsT f = FleetRcAssociationDailyStatsId (B.C f Data.Text.Text) (B.C f Data.Time.Calendar.Day) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetRcAssociationDailyStatsId <$> fleetOwnerId <*> merchantLocalDate <*> rcId

type FleetRcAssociationDailyStats = FleetRcAssociationDailyStatsT Identity

$(enableKVPG (''FleetRcAssociationDailyStatsT) [('fleetOwnerId), ('merchantLocalDate), ('rcId)] [])

$(mkTableInstances (''FleetRcAssociationDailyStatsT) "fleet_rc_association_daily_stats")
