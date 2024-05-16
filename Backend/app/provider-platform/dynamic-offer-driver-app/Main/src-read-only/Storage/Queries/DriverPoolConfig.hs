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
import qualified Storage.Beam.DriverPoolConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverPoolConfig.DriverPoolConfig] -> m ())
createMany = traverse_ create

findAllByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.DriverPoolConfig.DriverPoolConfig]))
findAllByMerchantOpCityId limit offset (Kernel.Types.Id.Id merchantOperatingCityId) = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc Beam.tripDistance) limit offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverPoolConfig.DriverPoolConfig -> m (Maybe Domain.Types.DriverPoolConfig.DriverPoolConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverPoolConfig.DriverPoolConfig -> m ())
updateByPrimaryKey (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualDistanceThreshold (Kernel.Prelude.fmap Kernel.Types.Common.distanceToMeters actualDistanceThreshold),
      Se.Set Beam.actualDistanceThresholdValue (Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance)) actualDistanceThreshold),
      Se.Set Beam.area area,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.distanceBasedBatchSplit distanceBasedBatchSplit,
      Se.Set Beam.driverBatchSize driverBatchSize,
      Se.Set Beam.driverPositionInfoExpiry driverPositionInfoExpiry,
      Se.Set Beam.driverQuoteLimit driverQuoteLimit,
      Se.Set Beam.driverRequestCountLimit driverRequestCountLimit,
      Se.Set Beam.driverToDestinationDistanceThreshold (Kernel.Types.Common.distanceToMeters driverToDestinationDistanceThreshold),
      Se.Set Beam.driverToDestinationDistanceThresholdValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) driverToDestinationDistanceThreshold),
      Se.Set Beam.driverToDestinationDuration driverToDestinationDuration,
      Se.Set Beam.maxDriverQuotesRequired maxDriverQuotesRequired,
      Se.Set Beam.maxNumberOfBatches maxNumberOfBatches,
      Se.Set Beam.maxParallelSearchRequests maxParallelSearchRequests,
      Se.Set Beam.maxRadiusOfSearch (Kernel.Types.Common.distanceToMeters maxRadiusOfSearch),
      Se.Set Beam.maxRadiusOfSearchValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) maxRadiusOfSearch),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minRadiusOfSearch (Kernel.Types.Common.distanceToMeters minRadiusOfSearch),
      Se.Set Beam.minRadiusOfSearchValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) minRadiusOfSearch),
      Se.Set Beam.poolSortingType poolSortingType,
      Se.Set Beam.radiusShrinkValueForDriversOnRide (Kernel.Types.Common.distanceToMeters radiusShrinkValueForDriversOnRide),
      Se.Set Beam.radiusShrinkValueForDriversOnRideValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) radiusShrinkValueForDriversOnRide),
      Se.Set Beam.radiusStepSize (Kernel.Types.Common.distanceToMeters radiusStepSize),
      Se.Set Beam.radiusStepSizeValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) radiusStepSize),
      Se.Set Beam.scheduleTryTimes scheduleTryTimes,
      Se.Set Beam.singleBatchProcessTime singleBatchProcessTime,
      Se.Set Beam.thresholdToIgnoreActualDistanceThreshold (Kernel.Prelude.fmap Kernel.Types.Common.distanceToMeters thresholdToIgnoreActualDistanceThreshold),
      Se.Set Beam.thresholdToIgnoreActualDistanceThresholdValue (Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance)) thresholdToIgnoreActualDistanceThreshold),
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just $ (.unit) tripDistance),
      Se.Set Beam.tripDistance (Kernel.Types.Common.distanceToMeters tripDistance),
      Se.Set Beam.tripDistanceValue (Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) tripDistance),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleVariant vehicleVariant
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  fromTType' (Beam.DriverPoolConfigT {..}) = do
    pure $
      Just
        Domain.Types.DriverPoolConfig.DriverPoolConfig
          { actualDistanceThreshold = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit actualDistanceThresholdValue <$> actualDistanceThreshold,
            area = area,
            createdAt = createdAt,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            driverBatchSize = driverBatchSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverToDestinationDistanceThreshold = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit driverToDestinationDistanceThresholdValue driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            id = Kernel.Types.Id.Id id,
            maxDriverQuotesRequired = maxDriverQuotesRequired,
            maxNumberOfBatches = maxNumberOfBatches,
            maxParallelSearchRequests = maxParallelSearchRequests,
            maxRadiusOfSearch = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit maxRadiusOfSearchValue maxRadiusOfSearch,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minRadiusOfSearch = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit minRadiusOfSearchValue minRadiusOfSearch,
            poolSortingType = poolSortingType,
            radiusShrinkValueForDriversOnRide = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit radiusShrinkValueForDriversOnRideValue radiusShrinkValueForDriversOnRide,
            radiusStepSize = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit radiusStepSizeValue radiusStepSize,
            scheduleTryTimes = scheduleTryTimes,
            singleBatchProcessTime = singleBatchProcessTime,
            thresholdToIgnoreActualDistanceThreshold = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit thresholdToIgnoreActualDistanceThresholdValue <$> thresholdToIgnoreActualDistanceThreshold,
            tripCategory = tripCategory,
            tripDistance = Kernel.Types.Common.mkDistanceWithDefaultMeters distanceUnit tripDistanceValue tripDistance,
            updatedAt = updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' Beam.DriverPoolConfig Domain.Types.DriverPoolConfig.DriverPoolConfig where
  toTType' (Domain.Types.DriverPoolConfig.DriverPoolConfig {..}) = do
    Beam.DriverPoolConfigT
      { Beam.actualDistanceThreshold = Kernel.Prelude.fmap Kernel.Types.Common.distanceToMeters actualDistanceThreshold,
        Beam.actualDistanceThresholdValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance)) actualDistanceThreshold,
        Beam.area = area,
        Beam.createdAt = createdAt,
        Beam.distanceBasedBatchSplit = distanceBasedBatchSplit,
        Beam.driverBatchSize = driverBatchSize,
        Beam.driverPositionInfoExpiry = driverPositionInfoExpiry,
        Beam.driverQuoteLimit = driverQuoteLimit,
        Beam.driverRequestCountLimit = driverRequestCountLimit,
        Beam.driverToDestinationDistanceThreshold = Kernel.Types.Common.distanceToMeters driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDistanceThresholdValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) driverToDestinationDistanceThreshold,
        Beam.driverToDestinationDuration = driverToDestinationDuration,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.maxDriverQuotesRequired = maxDriverQuotesRequired,
        Beam.maxNumberOfBatches = maxNumberOfBatches,
        Beam.maxParallelSearchRequests = maxParallelSearchRequests,
        Beam.maxRadiusOfSearch = Kernel.Types.Common.distanceToMeters maxRadiusOfSearch,
        Beam.maxRadiusOfSearchValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) maxRadiusOfSearch,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minRadiusOfSearch = Kernel.Types.Common.distanceToMeters minRadiusOfSearch,
        Beam.minRadiusOfSearchValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) minRadiusOfSearch,
        Beam.poolSortingType = poolSortingType,
        Beam.radiusShrinkValueForDriversOnRide = Kernel.Types.Common.distanceToMeters radiusShrinkValueForDriversOnRide,
        Beam.radiusShrinkValueForDriversOnRideValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) radiusShrinkValueForDriversOnRide,
        Beam.radiusStepSize = Kernel.Types.Common.distanceToMeters radiusStepSize,
        Beam.radiusStepSizeValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) radiusStepSize,
        Beam.scheduleTryTimes = scheduleTryTimes,
        Beam.singleBatchProcessTime = singleBatchProcessTime,
        Beam.thresholdToIgnoreActualDistanceThreshold = Kernel.Prelude.fmap Kernel.Types.Common.distanceToMeters thresholdToIgnoreActualDistanceThreshold,
        Beam.thresholdToIgnoreActualDistanceThresholdValue = Kernel.Prelude.fmap (Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance)) thresholdToIgnoreActualDistanceThreshold,
        Beam.tripCategory = tripCategory,
        Beam.distanceUnit = Kernel.Prelude.Just $ (.unit) tripDistance,
        Beam.tripDistance = Kernel.Types.Common.distanceToMeters tripDistance,
        Beam.tripDistanceValue = Kernel.Prelude.Just $ Kernel.Types.Common.distanceToHighPrecDistance (Kernel.Prelude.Just $ (.unit) tripDistance) tripDistance,
        Beam.updatedAt = updatedAt,
        Beam.vehicleVariant = vehicleVariant
      }
