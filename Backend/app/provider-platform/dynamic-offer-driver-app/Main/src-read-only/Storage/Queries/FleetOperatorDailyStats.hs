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

findByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats))
findByFleetOperatorIdAndDate fleetOperatorId fleetDriverId merchantLocalDate = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateByFleetOperatorIdAndDate totalRatingCount totalRatingScore driverFirstSubscription inspectionCompleted rejectedRequestCount pulledRequestCount acceptationRequestCount totalRequestCount customerCancellationCount driverCancellationCount totalDistance totalCompletedRides onlineTotalEarning cashTotalEarning cashPlatformFees onlinePlatformFees onlineDuration rideDuration fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.driverFirstSubscription driverFirstSubscription,
      Se.Set Beam.inspectionCompleted inspectionCompleted,
      Se.Set Beam.rejectedRequestCount rejectedRequestCount,
      Se.Set Beam.pulledRequestCount pulledRequestCount,
      Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.driverCancellationCount driverCancellationCount,
      Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.onlineTotalEarning onlineTotalEarning,
      Se.Set Beam.cashTotalEarning cashTotalEarning,
      Se.Set Beam.cashPlatformFees cashPlatformFees,
      Se.Set Beam.onlinePlatformFees onlinePlatformFees,
      Se.Set Beam.onlineDuration onlineDuration,
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateCustomerCancellationCountByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateCustomerCancellationCountByFleetOperatorIdAndDate customerCancellationCount fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateDistanceEarningAndCompletedRidesByFleetOperatorIdAndDate totalDistance onlineTotalEarning cashTotalEarning totalCompletedRides cashPlatformFees onlinePlatformFees rideDuration fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.onlineTotalEarning onlineTotalEarning,
      Se.Set Beam.cashTotalEarning cashTotalEarning,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.cashPlatformFees cashPlatformFees,
      Se.Set Beam.onlinePlatformFees onlinePlatformFees,
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateDriverCancellationCountByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateDriverCancellationCountByFleetOperatorIdAndDate driverCancellationCount fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.driverCancellationCount driverCancellationCount, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateDriverFirstSubscriptionByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateDriverFirstSubscriptionByFleetOperatorIdAndDate driverFirstSubscription fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.driverFirstSubscription driverFirstSubscription, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateInspectionCompletedByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateInspectionCompletedByFleetOperatorIdAndDate inspectionCompleted fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.inspectionCompleted inspectionCompleted, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateOnlineDurationByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateOnlineDurationByFleetOperatorIdAndDate onlineDuration fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.onlineDuration onlineDuration, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateRequestCountsByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateRequestCountsByFleetOperatorIdAndDate rejectedRequestCount pulledRequestCount acceptationRequestCount totalRequestCount fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.rejectedRequestCount rejectedRequestCount,
      Se.Set Beam.pulledRequestCount pulledRequestCount,
      Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m ())
updateTotalRatingCountAndTotalRatingScoreByFleetOperatorIdAndDate totalRatingCount totalRatingScore fleetOperatorId fleetDriverId merchantLocalDate = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Data.Time.Calendar.Day -> m (Maybe Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats))
findByPrimaryKey fleetDriverId fleetOperatorId merchantLocalDate = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats -> m ())
updateByPrimaryKey (Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acceptationRequestCount acceptationRequestCount,
      Se.Set Beam.cashPlatformFees cashPlatformFees,
      Se.Set Beam.cashTotalEarning cashTotalEarning,
      Se.Set Beam.currency currency,
      Se.Set Beam.customerCancellationCount customerCancellationCount,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.driverCancellationCount driverCancellationCount,
      Se.Set Beam.driverFirstSubscription driverFirstSubscription,
      Se.Set Beam.inspectionCompleted inspectionCompleted,
      Se.Set Beam.onlineDuration onlineDuration,
      Se.Set Beam.onlinePlatformFees onlinePlatformFees,
      Se.Set Beam.onlineTotalEarning onlineTotalEarning,
      Se.Set Beam.pulledRequestCount pulledRequestCount,
      Se.Set Beam.rejectedRequestCount rejectedRequestCount,
      Se.Set Beam.rideDuration rideDuration,
      Se.Set Beam.totalCompletedRides totalCompletedRides,
      Se.Set Beam.totalDistance (getTotalDistance totalDistance),
      Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.totalRequestCount totalRequestCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.fleetDriverId $ Se.Eq fleetDriverId,
          Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId,
          Se.Is Beam.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

instance FromTType' Beam.FleetOperatorDailyStats Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats where
  fromTType' (Beam.FleetOperatorDailyStatsT {..}) = do
    pure $
      Just
        Domain.Types.FleetOperatorDailyStats.FleetOperatorDailyStats
          { acceptationRequestCount = acceptationRequestCount,
            cashPlatformFees = cashPlatformFees,
            cashTotalEarning = cashTotalEarning,
            currency = currency,
            customerCancellationCount = customerCancellationCount,
            distanceUnit = distanceUnit,
            driverCancellationCount = driverCancellationCount,
            driverFirstSubscription = driverFirstSubscription,
            fleetDriverId = fleetDriverId,
            fleetOperatorId = fleetOperatorId,
            inspectionCompleted = inspectionCompleted,
            merchantLocalDate = merchantLocalDate,
            onlineDuration = onlineDuration,
            onlinePlatformFees = onlinePlatformFees,
            onlineTotalEarning = onlineTotalEarning,
            pulledRequestCount = pulledRequestCount,
            rejectedRequestCount = rejectedRequestCount,
            rideDuration = rideDuration,
            totalCompletedRides = totalCompletedRides,
            totalDistance = Kernel.Prelude.fmap (Kernel.Types.Common.Meters . GHC.Float.double2Int) totalDistance,
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
        Beam.cashPlatformFees = cashPlatformFees,
        Beam.cashTotalEarning = cashTotalEarning,
        Beam.currency = currency,
        Beam.customerCancellationCount = customerCancellationCount,
        Beam.distanceUnit = distanceUnit,
        Beam.driverCancellationCount = driverCancellationCount,
        Beam.driverFirstSubscription = driverFirstSubscription,
        Beam.fleetDriverId = fleetDriverId,
        Beam.fleetOperatorId = fleetOperatorId,
        Beam.inspectionCompleted = inspectionCompleted,
        Beam.merchantLocalDate = merchantLocalDate,
        Beam.onlineDuration = onlineDuration,
        Beam.onlinePlatformFees = onlinePlatformFees,
        Beam.onlineTotalEarning = onlineTotalEarning,
        Beam.pulledRequestCount = pulledRequestCount,
        Beam.rejectedRequestCount = rejectedRequestCount,
        Beam.rideDuration = rideDuration,
        Beam.totalCompletedRides = totalCompletedRides,
        Beam.totalDistance = getTotalDistance totalDistance,
        Beam.totalRatingCount = totalRatingCount,
        Beam.totalRatingScore = totalRatingScore,
        Beam.totalRequestCount = totalRequestCount,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
