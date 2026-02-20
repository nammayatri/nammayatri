{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverStats where

import qualified Domain.Types.DriverStats
import qualified GHC.Float
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverStats as Beam
import qualified Storage.Queries.Transformers.DailyStats
import Storage.Queries.Transformers.DriverStats
import qualified Storage.Queries.Transformers.DriverStats

instance FromTType' Beam.DriverStats Domain.Types.DriverStats.DriverStats where
  fromTType' (Beam.DriverStatsT {..}) = do
    rating' <- Storage.Queries.Transformers.DriverStats.getRating totalRatings totalRatingScore
    pure $
      Just
        Domain.Types.DriverStats.DriverStats
          { blacklistCoinEvents = blacklistCoinEvents,
            bonusEarned = Kernel.Types.Common.mkAmountWithDefault bonusEarnedAmount bonusEarned,
            coinCovertedToCashLeft = Kernel.Prelude.fromMaybe 0 coinCovertedToCashLeft,
            coinsConvertedToDirectPayoutCash = Kernel.Prelude.fromMaybe 0 coinsConvertedToDirectPayoutCash,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id driverId,
            earningsMissed = Kernel.Types.Common.mkAmountWithDefault earningsMissedAmount earningsMissed,
            favRiderCount = favRiderCount,
            idleSince = idleSince,
            isValidRating = isValidRating,
            lateNightTrips = lateNightTrips,
            numDriversOnboarded = Kernel.Prelude.fromMaybe 0 numDriversOnboarded,
            numFleetsOnboarded = Kernel.Prelude.fromMaybe 0 numFleetsOnboarded,
            onlineDuration = Kernel.Prelude.fromMaybe 0 onlineDuration,
            rating = rating',
            ridesCancelled = ridesCancelled,
            safetyPlusEarnings = Kernel.Prelude.fromMaybe 0 safetyPlusEarnings,
            safetyPlusRideCount = Kernel.Prelude.fromMaybe 0 safetyPlusRideCount,
            totalCoinsConvertedCash = Kernel.Prelude.fromMaybe 0 totalCoinsConvertedCash,
            totalDistance = Kernel.Types.Common.Meters $ GHC.Float.double2Int totalDistance,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            totalPayoutAmountPaid = totalPayoutAmountPaid,
            totalPayoutEarnings = Storage.Queries.Transformers.DailyStats.getHighPrecMoney totalPayoutEarnings,
            totalRatingScore = totalRatingScore,
            totalRatings = totalRatings,
            totalReferralCounts = Kernel.Prelude.fromMaybe 0 totalReferralCounts,
            totalRides = totalRides,
            totalRidesAssigned = totalRidesAssigned,
            totalValidActivatedRides = Kernel.Prelude.fromMaybe 0 totalValidActivatedRides,
            updatedAt = updatedAt,
            validCancellationTagsStatsStartDate = validCancellationTagsStatsStartDate,
            validCustomerCancellationTagCount = Kernel.Prelude.fromMaybe 0 validCustomerCancellationTagCount,
            validDriverCancellationTagCount = Kernel.Prelude.fromMaybe 0 validDriverCancellationTagCount
          }

instance ToTType' Beam.DriverStats Domain.Types.DriverStats.DriverStats where
  toTType' (Domain.Types.DriverStats.DriverStats {..}) = do
    Beam.DriverStatsT
      { Beam.blacklistCoinEvents = blacklistCoinEvents,
        Beam.bonusEarned = Kernel.Prelude.roundToIntegral bonusEarned,
        Beam.bonusEarnedAmount = Kernel.Prelude.Just bonusEarned,
        Beam.coinCovertedToCashLeft = Kernel.Prelude.Just coinCovertedToCashLeft,
        Beam.coinsConvertedToDirectPayoutCash = Kernel.Prelude.Just coinsConvertedToDirectPayoutCash,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.earningsMissed = Kernel.Prelude.roundToIntegral earningsMissed,
        Beam.earningsMissedAmount = Kernel.Prelude.Just earningsMissed,
        Beam.favRiderCount = favRiderCount,
        Beam.idleSince = idleSince,
        Beam.isValidRating = isValidRating,
        Beam.lateNightTrips = lateNightTrips,
        Beam.numDriversOnboarded = Kernel.Prelude.Just numDriversOnboarded,
        Beam.numFleetsOnboarded = Kernel.Prelude.Just numFleetsOnboarded,
        Beam.onlineDuration = Kernel.Prelude.Just onlineDuration,
        Beam.ridesCancelled = ridesCancelled,
        Beam.safetyPlusEarnings = Kernel.Prelude.Just safetyPlusEarnings,
        Beam.safetyPlusRideCount = Kernel.Prelude.Just safetyPlusRideCount,
        Beam.totalCoinsConvertedCash = Kernel.Prelude.Just totalCoinsConvertedCash,
        Beam.totalDistance = getTotalDistance totalDistance,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.totalPayoutAmountPaid = totalPayoutAmountPaid,
        Beam.totalPayoutEarnings = Kernel.Prelude.Just totalPayoutEarnings,
        Beam.totalRatingScore = totalRatingScore,
        Beam.totalRatings = totalRatings,
        Beam.totalReferralCounts = Kernel.Prelude.Just totalReferralCounts,
        Beam.totalRides = totalRides,
        Beam.totalRidesAssigned = totalRidesAssigned,
        Beam.totalValidActivatedRides = Kernel.Prelude.Just totalValidActivatedRides,
        Beam.updatedAt = updatedAt,
        Beam.validCancellationTagsStatsStartDate = validCancellationTagsStatsStartDate,
        Beam.validCustomerCancellationTagCount = Kernel.Prelude.Just validCustomerCancellationTagCount,
        Beam.validDriverCancellationTagCount = Kernel.Prelude.Just validDriverCancellationTagCount
      }
