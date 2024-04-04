{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DailyStats where

import qualified Domain.Types.DailyStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DailyStats as Beam

instance FromTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  fromTType' (Beam.DailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.DailyStats.DailyStats
          { driverId = Kernel.Types.Id.Id driverId,
            id = id,
            merchantLocalDate = merchantLocalDate,
            numRides = numRides,
            totalDistance = totalDistance,
            totalEarnings = totalEarnings,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DailyStats Domain.Types.DailyStats.DailyStats where
  toTType' (Domain.Types.DailyStats.DailyStats {..}) = do
    Beam.DailyStatsT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = id,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.numRides = numRides,
        Beam.totalDistance = totalDistance,
        Beam.totalEarnings = totalEarnings,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
