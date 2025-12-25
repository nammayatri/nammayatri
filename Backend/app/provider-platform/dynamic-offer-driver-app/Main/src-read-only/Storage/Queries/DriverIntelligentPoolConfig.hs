{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverIntelligentPoolConfig where

import qualified Domain.Types.DriverIntelligentPoolConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverIntelligentPoolConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig] -> m ())
createMany = traverse_ create

findByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig))
findByMerchantOpCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

update :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig -> m ())
update (Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig {..}) = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.availabilityTimeWeightage availabilityTimeWeightage,
      Se.Set Beam.availabilityTimeWindowOption availabilityTimeWindowOption,
      Se.Set Beam.acceptanceRatioWeightage acceptanceRatioWeightage,
      Se.Set Beam.acceptanceRatioWindowOption acceptanceRatioWindowOption,
      Se.Set Beam.cancellationRatioWeightage cancellationRatioWeightage,
      Se.Set Beam.cancellationAndRideFrequencyRatioWindowOption cancellationAndRideFrequencyRatioWindowOption,
      Se.Set Beam.minQuotesToQualifyForIntelligentPool minQuotesToQualifyForIntelligentPool,
      Se.Set Beam.minQuotesToQualifyForIntelligentPoolWindowOption minQuotesToQualifyForIntelligentPoolWindowOption,
      Se.Set Beam.intelligentPoolPercentage intelligentPoolPercentage,
      Se.Set Beam.speedNormalizer speedNormalizer,
      Se.Set Beam.driverSpeedWeightage driverSpeedWeightage,
      Se.Set Beam.minLocationUpdates minLocationUpdates,
      Se.Set Beam.locationUpdateSampleTime locationUpdateSampleTime,
      Se.Set Beam.defaultDriverSpeed defaultDriverSpeed,
      Se.Set Beam.updatedAt _now
    ]
    []

instance FromTType' Beam.DriverIntelligentPoolConfig Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig where
  fromTType' (Beam.DriverIntelligentPoolConfigT {..}) = do
    pure $
      Just
        Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig
          { acceptanceRatioWeightage = acceptanceRatioWeightage,
            acceptanceRatioWindowOption = acceptanceRatioWindowOption,
            actualPickupDistanceWeightage = actualPickupDistanceWeightage,
            availabilityTimeWeightage = availabilityTimeWeightage,
            availabilityTimeWindowOption = availabilityTimeWindowOption,
            cancellationAndRideFrequencyRatioWindowOption = cancellationAndRideFrequencyRatioWindowOption,
            cancellationRatioWeightage = cancellationRatioWeightage,
            createdAt = createdAt,
            defaultDriverSpeed = defaultDriverSpeed,
            driverSpeedWeightage = driverSpeedWeightage,
            intelligentPoolPercentage = intelligentPoolPercentage,
            locationUpdateSampleTime = locationUpdateSampleTime,
            maxNumRides = maxNumRides,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            minLocationUpdates = minLocationUpdates,
            minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
            minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
            numRidesWeightage = numRidesWeightage,
            speedNormalizer = speedNormalizer,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverIntelligentPoolConfig Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig where
  toTType' (Domain.Types.DriverIntelligentPoolConfig.DriverIntelligentPoolConfig {..}) = do
    Beam.DriverIntelligentPoolConfigT
      { Beam.acceptanceRatioWeightage = acceptanceRatioWeightage,
        Beam.acceptanceRatioWindowOption = acceptanceRatioWindowOption,
        Beam.actualPickupDistanceWeightage = actualPickupDistanceWeightage,
        Beam.availabilityTimeWeightage = availabilityTimeWeightage,
        Beam.availabilityTimeWindowOption = availabilityTimeWindowOption,
        Beam.cancellationAndRideFrequencyRatioWindowOption = cancellationAndRideFrequencyRatioWindowOption,
        Beam.cancellationRatioWeightage = cancellationRatioWeightage,
        Beam.createdAt = createdAt,
        Beam.defaultDriverSpeed = defaultDriverSpeed,
        Beam.driverSpeedWeightage = driverSpeedWeightage,
        Beam.intelligentPoolPercentage = intelligentPoolPercentage,
        Beam.locationUpdateSampleTime = locationUpdateSampleTime,
        Beam.maxNumRides = maxNumRides,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.minLocationUpdates = minLocationUpdates,
        Beam.minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
        Beam.minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
        Beam.numRidesWeightage = numRidesWeightage,
        Beam.speedNormalizer = speedNormalizer,
        Beam.updatedAt = updatedAt
      }
