{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetOperatorStats where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FleetOperatorStatsT f = FleetOperatorStatsT
  { acceptationRequestCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    customerCancellationCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    driverCancellationCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    driverFirstSubscription :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    fleetOperatorId :: B.C f Kernel.Prelude.Text,
    inspectionCompleted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalCompletedRides :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalDistance :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    totalEarning :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    totalRatingCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalRatingScore :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    totalRequestCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetOperatorStatsT where
  data PrimaryKey FleetOperatorStatsT f = FleetOperatorStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetOperatorStatsId . fleetOperatorId

type FleetOperatorStats = FleetOperatorStatsT Identity

$(enableKVPG ''FleetOperatorStatsT ['fleetOperatorId] [])

$(mkTableInstances ''FleetOperatorStatsT "fleet_operator_stats")
