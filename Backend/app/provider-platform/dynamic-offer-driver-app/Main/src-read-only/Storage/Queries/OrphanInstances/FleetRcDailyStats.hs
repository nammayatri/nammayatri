{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FleetRcDailyStats where

import qualified Domain.Types.FleetRcDailyStats
import qualified GHC.Float
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FleetRcDailyStats as Beam
import Storage.Queries.Transformers.FleetRcDailyStats

instance FromTType' Beam.FleetRcDailyStats Domain.Types.FleetRcDailyStats.FleetRcDailyStats where
  fromTType' (Beam.FleetRcDailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.FleetRcDailyStats.FleetRcDailyStats
          { currency = currency,
            fleetOwnerId = fleetOwnerId,
            merchantLocalDate = merchantLocalDate,
            rcId = rcId,
            rideDistance = Kernel.Types.Common.Meters $ GHC.Float.double2Int rideDistance,
            rideDuration = rideDuration,
            totalCompletedRides = totalCompletedRides,
            totalEarnings = totalEarnings,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetRcDailyStats Domain.Types.FleetRcDailyStats.FleetRcDailyStats where
  toTType' (Domain.Types.FleetRcDailyStats.FleetRcDailyStats {..}) = do
    Beam.FleetRcDailyStatsT
      { Beam.currency = currency,
        Beam.fleetOwnerId = fleetOwnerId,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.rcId = rcId,
        Beam.rideDistance = getRideDistance rideDistance,
        Beam.rideDuration = rideDuration,
        Beam.totalCompletedRides = totalCompletedRides,
        Beam.totalEarnings = totalEarnings,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
