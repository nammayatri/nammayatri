{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverPoolConfig where

import qualified Domain.Types.DriverPoolConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import qualified Storage.Beam.DriverPoolConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.DriverPoolConfig.DriverPoolConfig] -> m ()
createMany = traverse_ createWithKV

findAllByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.DriverPoolConfig.DriverPoolConfig])
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do
  findAllWithOptionsKV
    [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
    ]
    (Se.Desc Beam.tripDistance)
    limit
    offset

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Types.Common.Meters -> Kernel.Prelude.Text -> m (Maybe (Domain.Types.DriverPoolConfig.DriverPoolConfig))
findByPrimaryKey (Kernel.Types.Id.Id merchantOperatingCityId) tripCategory tripDistance vehicleVariant = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.tripDistance $ Se.Eq tripDistance,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ()
updateByPrimaryKey Domain.Types.DriverPoolConfig.DriverPoolConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistanceThreshold $ actualDistanceThreshold,
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.distanceBasedBatchSplit $ distanceBasedBatchSplit,
      Se.Set Beam.driverBatchSize $ driverBatchSize,
      Se.Set Beam.driverPositionInfoExpiry $ driverPositionInfoExpiry,
      Se.Set Beam.driverQuoteLimit $ driverQuoteLimit,
      Se.Set Beam.driverRequestCountLimit $ driverRequestCountLimit,
      Se.Set Beam.driverToDestinationDistanceThreshold $ driverToDestinationDistanceThreshold,
      Se.Set Beam.driverToDestinationDuration $ driverToDestinationDuration,
      Se.Set Beam.maxDriverQuotesRequired $ maxDriverQuotesRequired,
      Se.Set Beam.maxNumberOfBatches $ maxNumberOfBatches,
      Se.Set Beam.maxParallelSearchRequests $ maxParallelSearchRequests,
      Se.Set Beam.maxRadiusOfSearch $ maxRadiusOfSearch,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.minRadiusOfSearch $ minRadiusOfSearch,
      Se.Set Beam.poolSortingType $ poolSortingType,
      Se.Set Beam.radiusShrinkValueForDriversOnRide $ radiusShrinkValueForDriversOnRide,
      Se.Set Beam.radiusStepSize $ radiusStepSize,
      Se.Set Beam.singleBatchProcessTime $ singleBatchProcessTime,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.tripCategory $ Se.Eq tripCategory,
          Se.Is Beam.tripDistance $ Se.Eq tripDistance,
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

instance FromTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  fromTType' Beam.DriverPoolConfigT {..} = do
    pure $
      Just
        Domain.Types.DriverPoolConfig.DriverPoolConfig
          { actualDistanceThreshold = actualDistanceThreshold,
            createdAt = createdAt,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            driverBatchSize = driverBatchSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            maxDriverQuotesRequired = maxDriverQuotesRequired,
            maxNumberOfBatches = maxNumberOfBatches,
            maxParallelSearchRequests = maxParallelSearchRequests,
            maxRadiusOfSearch = maxRadiusOfSearch,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minRadiusOfSearch = minRadiusOfSearch,
            poolSortingType = poolSortingType,
            radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
            radiusStepSize = radiusStepSize,
            singleBatchProcessTime = singleBatchProcessTime,
            tripCategory = tripCategory,
            tripDistance = tripDistance,
            updatedAt = updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  toTType' Domain.Types.DriverPoolConfig.DriverPoolConfig {..} = do
    Beam.DriverPoolConfigT
      { Beam.actualDistanceThreshold = actualDistanceThreshold,
        Beam.createdAt = createdAt,
        Beam.distanceBasedBatchSplit = distanceBasedBatchSplit,
        Beam.driverBatchSize = driverBatchSize,
        Beam.driverPositionInfoExpiry = driverPositionInfoExpiry,
        Beam.driverQuoteLimit = driverQuoteLimit,
        Beam.driverRequestCountLimit = driverRequestCountLimit,
        Beam.driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDuration = driverToDestinationDuration,
        Beam.maxDriverQuotesRequired = maxDriverQuotesRequired,
        Beam.maxNumberOfBatches = maxNumberOfBatches,
        Beam.maxParallelSearchRequests = maxParallelSearchRequests,
        Beam.maxRadiusOfSearch = maxRadiusOfSearch,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minRadiusOfSearch = minRadiusOfSearch,
        Beam.poolSortingType = poolSortingType,
        Beam.radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
        Beam.radiusStepSize = radiusStepSize,
        Beam.singleBatchProcessTime = singleBatchProcessTime,
        Beam.tripCategory = tripCategory,
        Beam.tripDistance = tripDistance,
        Beam.updatedAt = updatedAt,
        Beam.vehicleVariant = vehicleVariant
      }
