{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DriverStats where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Lib.DriverCoins.Types
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data DriverStatsT f
    = DriverStatsT {blacklistCoinEvents :: (B.C f (Kernel.Prelude.Maybe [Lib.DriverCoins.Types.DriverCoinsFunctionType])),
                    bonusEarned :: (B.C f Kernel.Types.Common.Money),
                    bonusEarnedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    coinCovertedToCashLeft :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    coinsConvertedToDirectPayoutCash :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                    d2cReferralCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    d2dReferralCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
                    driverId :: (B.C f Kernel.Prelude.Text),
                    earningsMissed :: (B.C f Kernel.Types.Common.Money),
                    earningsMissedAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    favRiderCount :: (B.C f Kernel.Prelude.Int),
                    idleSince :: (B.C f Kernel.Prelude.UTCTime),
                    isValidRating :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                    lateNightTrips :: (B.C f Kernel.Prelude.Int),
                    numDriversOnboarded :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    numFleetsOnboarded :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    onlineDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
                    ridesCancelled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    safetyPlusEarnings :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    safetyPlusRideCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    totalCoinsConvertedCash :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    totalDistance :: (B.C f Kernel.Prelude.Double),
                    totalEarnings :: (B.C f Kernel.Types.Common.Money),
                    totalEarningsAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    totalPayoutAmountPaid :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    totalPayoutEarnings :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                    totalRatingScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    totalRatings :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    totalReferralCounts :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    totalRides :: (B.C f Kernel.Prelude.Int),
                    totalRidesAssigned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    totalValidActivatedRides :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                    validCancellationTagsStatsStartDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                    validCustomerCancellationTagCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                    validDriverCancellationTagCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int))}
    deriving (Generic, B.Beamable)
instance B.Table DriverStatsT
    where data PrimaryKey DriverStatsT f = DriverStatsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DriverStatsId . driverId
type DriverStats = DriverStatsT Identity

$(enableKVPG (''DriverStatsT) [('driverId)] [])

$(mkTableInstances (''DriverStatsT) "driver_stats")

