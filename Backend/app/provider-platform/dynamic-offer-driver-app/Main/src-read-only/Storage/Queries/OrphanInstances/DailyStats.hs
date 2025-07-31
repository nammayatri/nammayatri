{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DailyStats where

import qualified Domain.Types.DailyStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DailyStats as Beam
import Storage.Queries.Transformers.DailyStats

instance FromTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  fromTType' (Beam.DailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.DailyStats.DailyStats
          { activatedValidRides = Kernel.Prelude.fromMaybe 0 activatedValidRides,
            bonusEarnings = Kernel.Prelude.fromMaybe 0 bonusEarnings,
            cancellationCharges = Kernel.Prelude.fromMaybe 0 cancellationCharges,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id driverId,
            id = id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantLocalDate = merchantLocalDate,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            numDriversOnboarded = Kernel.Prelude.fromMaybe 0 numDriversOnboarded,
            numFleetsOnboarded = Kernel.Prelude.fromMaybe 0 numFleetsOnboarded,
            numRides = numRides,
            onlineDuration = onlineDuration,
            payoutOrderId = payoutOrderId,
            payoutOrderStatus = payoutOrderStatus,
            payoutStatus = Kernel.Prelude.fromMaybe Domain.Types.DailyStats.Initialized payoutStatus,
            referralCounts = Kernel.Prelude.fromMaybe 0 referralCounts,
            referralEarnings = getHighPrecMoney referralEarnings,
            tipAmount = Kernel.Prelude.fromMaybe 0 tipAmount,
            tollCharges = Kernel.Prelude.fromMaybe 0 tollCharges,
            totalDistance = totalDistance,
            totalEarnings = Kernel.Types.Common.mkAmountWithDefault totalEarningsAmount totalEarnings,
            totalRideTime = Kernel.Prelude.fromMaybe 0 totalRideTime,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  toTType' (Domain.Types.DailyStats.DailyStats {..}) = do
    Beam.DailyStatsT
      { Beam.activatedValidRides = Kernel.Prelude.Just activatedValidRides,
        Beam.bonusEarnings = Kernel.Prelude.Just bonusEarnings,
        Beam.cancellationCharges = Kernel.Prelude.Just cancellationCharges,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.numDriversOnboarded = Kernel.Prelude.Just numDriversOnboarded,
        Beam.numFleetsOnboarded = Kernel.Prelude.Just numFleetsOnboarded,
        Beam.numRides = numRides,
        Beam.onlineDuration = onlineDuration,
        Beam.payoutOrderId = payoutOrderId,
        Beam.payoutOrderStatus = payoutOrderStatus,
        Beam.payoutStatus = Kernel.Prelude.Just payoutStatus,
        Beam.referralCounts = Kernel.Prelude.Just referralCounts,
        Beam.referralEarnings = Kernel.Prelude.Just referralEarnings,
        Beam.tipAmount = Kernel.Prelude.Just tipAmount,
        Beam.tollCharges = Kernel.Prelude.Just tollCharges,
        Beam.totalDistance = totalDistance,
        Beam.totalEarnings = Kernel.Prelude.roundToIntegral totalEarnings,
        Beam.totalEarningsAmount = Kernel.Prelude.Just totalEarnings,
        Beam.totalRideTime = Kernel.Prelude.Just totalRideTime,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
