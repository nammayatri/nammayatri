{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOperatorStats where

import qualified Domain.Types.FleetOperatorStats
import qualified GHC.Float
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorStats as Beam
import Storage.Queries.Transformers.FleetOperatorStats

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorStats.FleetOperatorStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOperatorStats.FleetOperatorStats] -> m ())
createMany = traverse_ create

deleteByFleetOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFleetOperatorId fleetOperatorId = do deleteWithKV [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateCustomerCancellationCountByFleetOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateCustomerCancellationCountByFleetOperatorId customerCancellationCount fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.customerCancellationCount customerCancellationCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateDistanceEarningAndCompletedRidesByFleetOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateDistanceEarningAndCompletedRidesByFleetOperatorId totalDistance totalEarning totalCompletedRides fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalEarning totalEarning,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateDriverCancellationCountByFleetOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateDriverCancellationCountByFleetOperatorId driverCancellationCount fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverCancellationCount driverCancellationCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateDriverFirstSubscriptionByFleetOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateDriverFirstSubscriptionByFleetOperatorId driverFirstSubscription fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverFirstSubscription driverFirstSubscription, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateInspectionCompletedByFleetOperatorId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateInspectionCompletedByFleetOperatorId inspectionCompleted fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.inspectionCompleted inspectionCompleted, Se.Set Beam.updatedAt _now] [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateRequestCountsByFleetOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateRequestCountsByFleetOperatorId acceptationRequestCount totalRequestCount fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

updateTotalRatingCountAndTotalRatingScoreByFleetOperatorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
updateTotalRatingCountAndTotalRatingScoreByFleetOperatorId totalRatingCount totalRatingScore fleetOperatorId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetOperatorStats.FleetOperatorStats))
findByPrimaryKey fleetOperatorId = do findOneWithKV [Se.And [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorStats.FleetOperatorStats -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorStats.FleetOperatorStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.currency currency,
      Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.driverCancellationCount driverCancellationCount,
      Se.Set Beam.driverFirstSubscription driverFirstSubscription,
      Se.Set Beam.inspectionCompleted inspectionCompleted,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalEarning totalEarning,
      Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]]

instance FromTType' Beam.FleetOperatorStats Domain.Types.FleetOperatorStats.FleetOperatorStats where
  fromTType' (Beam.FleetOperatorStatsT {..}) = do
    pure $
      Just
        Domain.Types.FleetOperatorStats.FleetOperatorStats
          { acceptationRequestCount = acceptationRequestCount,
            currency = currency,
            customerCancellationCount = customerCancellationCount,
            distanceUnit = distanceUnit,
            driverCancellationCount = driverCancellationCount,
            driverFirstSubscription = driverFirstSubscription,
            fleetOperatorId = fleetOperatorId,
            inspectionCompleted = inspectionCompleted,
            totalCompletedRides = totalCompletedRides,
            totalDistance = Kernel.Prelude.fmap (Kernel.Types.Common.Meters . GHC.Float.double2Int) totalDistance,
            totalEarning = totalEarning,
            totalRatingCount = totalRatingCount,
            totalRatingScore = totalRatingScore,
            totalRequestCount = totalRequestCount,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FleetOperatorStats Domain.Types.FleetOperatorStats.FleetOperatorStats where
  toTType' (Domain.Types.FleetOperatorStats.FleetOperatorStats {..}) = do
    Beam.FleetOperatorStatsT
      { Beam.acceptationRequestCount = acceptationRequestCount,
        Beam.currency = currency,
        Beam.customerCancellationCount = customerCancellationCount,
        Beam.distanceUnit = distanceUnit,
        Beam.driverCancellationCount = driverCancellationCount,
        Beam.driverFirstSubscription = driverFirstSubscription,
        Beam.fleetOperatorId = fleetOperatorId,
        Beam.inspectionCompleted = inspectionCompleted,
        Beam.totalCompletedRides = totalCompletedRides,
        Beam.totalDistance = getTotalDistance totalDistance,
        Beam.totalEarning = totalEarning,
        Beam.totalRatingCount = totalRatingCount,
        Beam.totalRatingScore = totalRatingScore,
        Beam.totalRequestCount = totalRequestCount,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
