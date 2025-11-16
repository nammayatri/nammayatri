{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPoolConfig where

import qualified Data.Vector
import qualified Domain.Types.DriverPoolConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverPoolConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverPoolConfig.DriverPoolConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DriverPoolConfig.DriverPoolConfig])
findAllByMerchantOpCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Desc Beam.tripDistance) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverPoolConfig.DriverPoolConfig -> m (Maybe Domain.Types.DriverPoolConfig.DriverPoolConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
updateByPrimaryKey (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistanceThreshold actualDistanceThreshold,
      Se.Set Beam.actualDistanceThresholdOnRide actualDistanceThresholdOnRide,
      Se.Set Beam.area area,
      Se.Set Beam.batchSizeOnRide batchSizeOnRide,
      Se.Set Beam.batchSizeOnRideWithStraightLineDistance batchSizeOnRideWithStraightLineDistance,
      Se.Set Beam.currentRideTripCategoryValidForForwardBatching currentRideTripCategoryValidForForwardBatching,
      Se.Set Beam.distanceBasedBatchSplit distanceBasedBatchSplit,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverBatchSize driverBatchSize,
      Se.Set Beam.driverPositionInfoExpiry driverPositionInfoExpiry,
      Se.Set Beam.driverQuoteLimit driverQuoteLimit,
      Se.Set Beam.driverRequestCountLimit driverRequestCountLimit,
      Se.Set Beam.driverToDestinationDistanceThreshold driverToDestinationDistanceThreshold,
      Se.Set Beam.driverToDestinationDuration driverToDestinationDuration,
      Se.Set Beam.dynamicBatchSize (Kernel.Prelude.Just $ Data.Vector.toList dynamicBatchSize),
      Se.Set Beam.enableForwardBatching enableForwardBatching,
      Se.Set Beam.enableUnifiedPooling enableUnifiedPooling,
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
      Se.Set Beam.useOneToOneOsrmMapping useOneToOneOsrmMapping,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  fromTType' (Beam.DriverPoolConfigT {..}) = do
    pure $
      Just
        Domain.Types.DriverPoolConfig.DriverPoolConfig
          { actualDistanceThreshold = actualDistanceThreshold,
            actualDistanceThresholdOnRide = actualDistanceThresholdOnRide,
            area = area,
            batchSizeOnRide = batchSizeOnRide,
            batchSizeOnRideWithStraightLineDistance = batchSizeOnRideWithStraightLineDistance,
            createdAt = createdAt,
            currentRideTripCategoryValidForForwardBatching = currentRideTripCategoryValidForForwardBatching,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverBatchSize = driverBatchSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            dynamicBatchSize = Kernel.Prelude.maybe (Data.Vector.singleton driverBatchSize) Data.Vector.fromList dynamicBatchSize,
            enableForwardBatching = enableForwardBatching,
            enableUnifiedPooling = enableUnifiedPooling,
            id = Kernel.Types.Id.Id id,
            maxDriverQuotesRequired = maxDriverQuotesRequired,
            maxNumberOfBatches = maxNumberOfBatches,
            maxParallelSearchRequests = maxParallelSearchRequests,
            maxParallelSearchRequestsOnRide = maxParallelSearchRequestsOnRide,
            maxRadiusOfSearch = maxRadiusOfSearch,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minRadiusOfSearch = minRadiusOfSearch,
            onRideBatchSplitConfig = onRideBatchSplitConfig,
            onRideRadiusConfig = onRideRadiusConfig,
            poolSortingType = poolSortingType,
            radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
            radiusStepSize = radiusStepSize,
            scheduleTryTimes = scheduleTryTimes,
            singleBatchProcessTime = singleBatchProcessTime,
            thresholdToIgnoreActualDistanceThreshold = thresholdToIgnoreActualDistanceThreshold,
            timeBounds = timeBounds,
            tripCategory = tripCategory,
            tripDistance = tripDistance,
            updatedAt = updatedAt,
            useOneToOneOsrmMapping = useOneToOneOsrmMapping,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  toTType' (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
    Beam.DriverPoolConfigT
      { Beam.actualDistanceThreshold = actualDistanceThreshold,
        Beam.actualDistanceThresholdOnRide = actualDistanceThresholdOnRide,
        Beam.area = area,
        Beam.batchSizeOnRide = batchSizeOnRide,
        Beam.batchSizeOnRideWithStraightLineDistance = batchSizeOnRideWithStraightLineDistance,
        Beam.createdAt = createdAt,
        Beam.currentRideTripCategoryValidForForwardBatching = currentRideTripCategoryValidForForwardBatching,
        Beam.distanceBasedBatchSplit = distanceBasedBatchSplit,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverBatchSize = driverBatchSize,
        Beam.driverPositionInfoExpiry = driverPositionInfoExpiry,
        Beam.driverQuoteLimit = driverQuoteLimit,
        Beam.driverRequestCountLimit = driverRequestCountLimit,
        Beam.driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDuration = driverToDestinationDuration,
        Beam.dynamicBatchSize = Kernel.Prelude.Just $ Data.Vector.toList dynamicBatchSize,
        Beam.enableForwardBatching = enableForwardBatching,
        Beam.enableUnifiedPooling = enableUnifiedPooling,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDriverQuotesRequired = maxDriverQuotesRequired,
        Beam.maxNumberOfBatches = maxNumberOfBatches,
        Beam.maxParallelSearchRequests = maxParallelSearchRequests,
        Beam.maxParallelSearchRequestsOnRide = maxParallelSearchRequestsOnRide,
        Beam.maxRadiusOfSearch = maxRadiusOfSearch,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minRadiusOfSearch = minRadiusOfSearch,
        Beam.onRideBatchSplitConfig = onRideBatchSplitConfig,
        Beam.onRideRadiusConfig = onRideRadiusConfig,
        Beam.poolSortingType = poolSortingType,
        Beam.radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
        Beam.radiusStepSize = radiusStepSize,
        Beam.scheduleTryTimes = scheduleTryTimes,
        Beam.singleBatchProcessTime = singleBatchProcessTime,
        Beam.thresholdToIgnoreActualDistanceThreshold = thresholdToIgnoreActualDistanceThreshold,
        Beam.timeBounds = timeBounds,
        Beam.tripCategory = tripCategory,
        Beam.tripDistance = tripDistance,
        Beam.updatedAt = updatedAt,
        Beam.useOneToOneOsrmMapping = useOneToOneOsrmMapping,
        Beam.vehicleVariant = vehicleVariant
      }
