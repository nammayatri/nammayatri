{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPoolConfig (module Storage.Queries.DriverPoolConfig, module ReExport) where

import qualified Domain.Types.DriverPoolConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPoolConfig as Beam
import Storage.Queries.DriverPoolConfigExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverPoolConfig.DriverPoolConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DriverPoolConfig.DriverPoolConfig])
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc Beam.tripDistance) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverPoolConfig.DriverPoolConfig -> m (Maybe Domain.Types.DriverPoolConfig.DriverPoolConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
updateByPrimaryKey (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistanceThreshold actualDistanceThreshold,
      Se.Set Beam.actualDistanceThresholdOnRide actualDistanceThresholdOnRide,
      Se.Set Beam.area area,
      Se.Set Beam.batchSizeOnRide batchSizeOnRide,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.currentRideTripCategoryValidForForwardBatching currentRideTripCategoryValidForForwardBatching,
      Se.Set Beam.distanceBasedBatchSplit distanceBasedBatchSplit,
      Se.Set Beam.driverBatchSize driverBatchSize,
      Se.Set Beam.driverPositionInfoExpiry driverPositionInfoExpiry,
      Se.Set Beam.driverQuoteLimit driverQuoteLimit,
      Se.Set Beam.driverRequestCountLimit driverRequestCountLimit,
      Se.Set Beam.driverToDestinationDistanceThreshold driverToDestinationDistanceThreshold,
      Se.Set Beam.driverToDestinationDuration driverToDestinationDuration,
      Se.Set Beam.enableForwardBatching enableForwardBatching,
      Se.Set Beam.maxDriverQuotesRequired maxDriverQuotesRequired,
      Se.Set Beam.maxNumberOfBatches maxNumberOfBatches,
      Se.Set Beam.maxParallelSearchRequests maxParallelSearchRequests,
      Se.Set Beam.maxParallelSearchRequestsOnRide maxParallelSearchRequestsOnRide,
      Se.Set Beam.maxRadiusOfSearch maxRadiusOfSearch,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minRadiusOfSearch minRadiusOfSearch,
      Se.Set Beam.onRideBatchSplitConfig onRideBatchSplitConfig,
      Se.Set Beam.onRideRadiusConfig onRideRadiusConfig,
      Se.Set Beam.poolSortingType poolSortingType,
      Se.Set Beam.radiusShrinkValueForDriversOnRide radiusShrinkValueForDriversOnRide,
      Se.Set Beam.radiusStepSize radiusStepSize,
      Se.Set Beam.scheduleTryTimes scheduleTryTimes,
      Se.Set Beam.singleBatchProcessTime singleBatchProcessTime,
      Se.Set Beam.thresholdToIgnoreActualDistanceThreshold thresholdToIgnoreActualDistanceThreshold,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.tripDistance tripDistance,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
