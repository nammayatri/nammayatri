{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetOperatorStats where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data FleetOperatorStatsT f
    = FleetOperatorStatsT {acceptationRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                           customerCancellationCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                           driverCancellationCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           driverFirstSubscription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           fleetOperatorId :: (B.C f Kernel.Prelude.Text),
                           inspectionCompleted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           totalCompletedRides :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           totalDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                           totalEarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                           totalRatingCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           totalRatingScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           totalRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                           merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                           merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetOperatorStatsT
    where data PrimaryKey FleetOperatorStatsT f = FleetOperatorStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FleetOperatorStatsId . fleetOperatorId
type FleetOperatorStats = FleetOperatorStatsT Identity

$(enableKVPG (''FleetOperatorStatsT) [('fleetOperatorId)] [])

$(mkTableInstances (''FleetOperatorStatsT) "fleet_operator_stats")

