{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.FleetRcDailyStats where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.FleetRcDailyStats
import qualified Domain.Types.FleetRcDailyStats
import qualified Storage.Beam.FleetRcDailyStats as Beam
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified GHC.Float



instance FromTType' Beam.FleetRcDailyStats Domain.Types.FleetRcDailyStats.FleetRcDailyStats
    where fromTType' (Beam.FleetRcDailyStatsT {..}) = do pure $ Just Domain.Types.FleetRcDailyStats.FleetRcDailyStats{currency = currency,
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
                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.FleetRcDailyStats Domain.Types.FleetRcDailyStats.FleetRcDailyStats
    where toTType' (Domain.Types.FleetRcDailyStats.FleetRcDailyStats {..}) = do Beam.FleetRcDailyStatsT{Beam.currency = currency,
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
                                                                                                        Beam.updatedAt = updatedAt}



