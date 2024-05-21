{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverPoolConfig where

import qualified Domain.Types.DriverPoolConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverPoolConfig as Beam

instance FromTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  fromTType' (Beam.DriverPoolConfigT {..}) = do
    pure $
      Just
        Domain.Types.DriverPoolConfig.DriverPoolConfig
          { actualDistanceThreshold = actualDistanceThreshold,
            actualDistanceThresholdOnRide = actualDistanceThresholdOnRide,
            area = area,
            batchSizeOnRide = batchSizeOnRide,
            createdAt = createdAt,
            currentRideTripCategoryValidForForwardBatching = currentRideTripCategoryValidForForwardBatching,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            driverBatchSize = driverBatchSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            enableForwardBatching = enableForwardBatching,
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
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  toTType' (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
    Beam.DriverPoolConfigT
      { Beam.actualDistanceThreshold = actualDistanceThreshold,
        Beam.actualDistanceThresholdOnRide = actualDistanceThresholdOnRide,
        Beam.area = area,
        Beam.batchSizeOnRide = batchSizeOnRide,
        Beam.createdAt = createdAt,
        Beam.currentRideTripCategoryValidForForwardBatching = currentRideTripCategoryValidForForwardBatching,
        Beam.distanceBasedBatchSplit = distanceBasedBatchSplit,
        Beam.driverBatchSize = driverBatchSize,
        Beam.driverPositionInfoExpiry = driverPositionInfoExpiry,
        Beam.driverQuoteLimit = driverQuoteLimit,
        Beam.driverRequestCountLimit = driverRequestCountLimit,
        Beam.driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDuration = driverToDestinationDuration,
        Beam.enableForwardBatching = enableForwardBatching,
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
        Beam.vehicleVariant = vehicleVariant
      }
