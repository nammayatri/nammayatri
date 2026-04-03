{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FleetOperatorDailyStats where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Data.Time.Calendar
import qualified Database.Beam as B



data FleetOperatorDailyStatsT f
    = FleetOperatorDailyStatsT {acceptationRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                cashPlatformFees :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                cashTotalEarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                                customerCancellationCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                                driverCancellationCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                driverFirstSubscription :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                fleetDriverId :: (B.C f Kernel.Prelude.Text),
                                fleetOperatorId :: (B.C f Kernel.Prelude.Text),
                                inspectionCompleted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                merchantLocalDate :: (B.C f Data.Time.Calendar.Day),
                                onlineDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
                                onlinePlatformFees :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                onlineTotalEarning :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                pulledRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                rejectedRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                rideDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
                                totalCompletedRides :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                totalDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
                                totalRatingCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                totalRatingScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                totalRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FleetOperatorDailyStatsT
    where data PrimaryKey FleetOperatorDailyStatsT f = FleetOperatorDailyStatsId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Data.Time.Calendar.Day) deriving (Generic, B.Beamable)
          primaryKey = FleetOperatorDailyStatsId <$> fleetDriverId <*> fleetOperatorId <*> merchantLocalDate
type FleetOperatorDailyStats = FleetOperatorDailyStatsT Identity

$(enableKVPG (''FleetOperatorDailyStatsT) [('fleetDriverId), ('fleetOperatorId), ('merchantLocalDate)] [])

$(mkTableInstances (''FleetOperatorDailyStatsT) "fleet_operator_daily_stats")

