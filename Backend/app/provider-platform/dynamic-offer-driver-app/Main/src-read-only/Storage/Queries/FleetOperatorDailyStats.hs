{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetOperatorDailyStats where

import qualified Data.Time.Calendar
import qualified Domain.Types.FleetOperatorDailyStats
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
import qualified Storage.Beam.FleetOperatorDailyStats as Beam
import Storage.Queries.Transformers.FleetOperatorDailyStats

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats] -> m ())
createMany = traverse_ create

findByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats))
findByFleetOperatorIdAndDate fleetOperatorId merchantLocalDate = do findOneWithKV [Se.And [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId, Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate]]

updateAcceptationRequestCountByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateAcceptationRequestCountByFleetOperatorIdAndDate acceptationRequestCount fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.acceptationRequestCount acceptationRequestCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateByFleetOperatorIdAndDate totalRatingCount totalRatingScore driverFirstSubscription inspectionCompleted acceptationRequestCount totalRequestCount customerCancellationCount driverCancellationCount totalDistance totalCompletedRides totalEarning fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.driverFirstSubscription driverFirstSubscription,
      Se.Set Beam.inspectionCompleted inspectionCompleted,
      Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.driverCancellationCount driverCancellationCount,
      Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalEarning totalEarning,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateCustomerCancellationCountByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateCustomerCancellationCountByFleetOperatorIdAndDate customerCancellationCount fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.customerCancellationCount customerCancellationCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateDriverCancellationCountByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateDriverCancellationCountByFleetOperatorIdAndDate driverCancellationCount fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.driverCancellationCount driverCancellationCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateDriverFirstSubscriptionByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateDriverFirstSubscriptionByFleetOperatorIdAndDate driverFirstSubscription fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.driverFirstSubscription driverFirstSubscription, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateInspectionCompletedByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateInspectionCompletedByFleetOperatorIdAndDate inspectionCompleted fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.inspectionCompleted inspectionCompleted, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalCompletedRidesByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalCompletedRidesByFleetOperatorIdAndDate totalCompletedRides fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalCompletedRides totalCompletedRides, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalDistanceByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalDistanceByFleetOperatorIdAndDate totalDistance fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalDistance (getTotalDistance totalDistance), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalEarningByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalEarningByFleetOperatorIdAndDate totalEarning fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalEarning totalEarning, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalRatingCountByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalRatingCountByFleetOperatorIdAndDate totalRatingCount fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalRatingCount totalRatingCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalRatingScoreByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalRatingScoreByFleetOperatorIdAndDate totalRatingScore fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalRatingScore totalRatingScore, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalRequestCountByFleetOperatorIdAndDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalRequestCountByFleetOperatorIdAndDate totalRequestCount fleetOperatorId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.totalRequestCount totalRequestCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.currency currency,
      Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.driverCancellationCount driverCancellationCount,
      Se.Set Beam.driverFirstSubscription driverFirstSubscription,
      Se.Set Beam.fleetOperatorId fleetOperatorId,
      Se.Set Beam.inspectionCompleted inspectionCompleted,
      Se.Set Beam.merchantLocalDate merchantLocalDate,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalEarning totalEarning,
      Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq id]]

instance FromTType' Beam.FleetOperatorDailyStats Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats where
  fromTType' (Beam.FleetOperatorDailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats
          { acceptationRequestCount = acceptationRequestCount,
            currency = currency,
            customerCancellationCount = customerCancellationCount,
            distanceUnit = distanceUnit,
            driverCancellationCount = driverCancellationCount,
            driverFirstSubscription = driverFirstSubscription,
            fleetOperatorId = fleetOperatorId,
            id = id,
            inspectionCompleted = inspectionCompleted,
            merchantLocalDate = merchantLocalDate,
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

instance ToTType' Beam.FleetOperatorDailyStats Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats where
  toTType' (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats {..}) = do
    Beam.FleetOperatorDailyStatsT
      { Beam.acceptationRequestCount = acceptationRequestCount,
        Beam.currency = currency,
        Beam.customerCancellationCount = customerCancellationCount,
        Beam.distanceUnit = distanceUnit,
        Beam.driverCancellationCount = driverCancellationCount,
        Beam.driverFirstSubscription = driverFirstSubscription,
        Beam.fleetOperatorId = fleetOperatorId,
        Beam.id = id,
        Beam.inspectionCompleted = inspectionCompleted,
        Beam.merchantLocalDate = merchantLocalDate,
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
