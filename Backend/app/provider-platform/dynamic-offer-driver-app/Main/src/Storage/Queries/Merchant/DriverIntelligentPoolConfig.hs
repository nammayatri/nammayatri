{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.DriverIntelligentPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.DriverIntelligentPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.DriverIntelligentPoolConfig as BeamDIPC

findByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantOpCityId (Id merchantOperatingCityId) = findOneWithKV [Se.Is BeamDIPC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverIntelligentPoolConfig -> m ()
update config = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamDIPC.availabilityTimeWeightage config.availabilityTimeWeightage,
      Se.Set BeamDIPC.availabilityTimeWindowOption config.availabilityTimeWindowOption,
      Se.Set BeamDIPC.acceptanceRatioWeightage config.acceptanceRatioWeightage,
      Se.Set BeamDIPC.acceptanceRatioWindowOption config.acceptanceRatioWindowOption,
      Se.Set BeamDIPC.cancellationRatioWeightage config.cancellationRatioWeightage,
      Se.Set BeamDIPC.cancellationRatioWindowOption config.cancellationRatioWindowOption,
      Se.Set BeamDIPC.minQuotesToQualifyForIntelligentPool config.minQuotesToQualifyForIntelligentPool,
      Se.Set BeamDIPC.minQuotesToQualifyForIntelligentPoolWindowOption config.minQuotesToQualifyForIntelligentPoolWindowOption,
      Se.Set BeamDIPC.intelligentPoolPercentage config.intelligentPoolPercentage,
      Se.Set BeamDIPC.speedNormalizer config.speedNormalizer,
      Se.Set BeamDIPC.driverSpeedWeightage config.driverSpeedWeightage,
      Se.Set BeamDIPC.minLocationUpdates config.minLocationUpdates,
      Se.Set BeamDIPC.locationUpdateSampleTime config.locationUpdateSampleTime,
      Se.Set BeamDIPC.defaultDriverSpeed config.defaultDriverSpeed,
      Se.Set BeamDIPC.updatedAt now
    ]
    [Se.Is BeamDIPC.merchantOperatingCityId (Se.Eq $ getId config.merchantOperatingCityId)]

instance FromTType' BeamDIPC.DriverIntelligentPoolConfig DriverIntelligentPoolConfig where
  fromTType' BeamDIPC.DriverIntelligentPoolConfigT {..} = do
    pure $
      Just
        DriverIntelligentPoolConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            actualPickupDistanceWeightage = actualPickupDistanceWeightage,
            availabilityTimeWeightage = availabilityTimeWeightage,
            availabilityTimeWindowOption = availabilityTimeWindowOption,
            acceptanceRatioWeightage = acceptanceRatioWeightage,
            acceptanceRatioWindowOption = acceptanceRatioWindowOption,
            cancellationRatioWeightage = cancellationRatioWeightage,
            cancellationRatioWindowOption = cancellationRatioWindowOption,
            minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
            minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
            intelligentPoolPercentage = intelligentPoolPercentage,
            speedNormalizer = speedNormalizer,
            driverSpeedWeightage = driverSpeedWeightage,
            minLocationUpdates = minLocationUpdates,
            locationUpdateSampleTime = locationUpdateSampleTime,
            defaultDriverSpeed = defaultDriverSpeed,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamDIPC.DriverIntelligentPoolConfig DriverIntelligentPoolConfig where
  toTType' DriverIntelligentPoolConfig {..} = do
    BeamDIPC.DriverIntelligentPoolConfigT
      { BeamDIPC.merchantId = getId merchantId,
        BeamDIPC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamDIPC.actualPickupDistanceWeightage = actualPickupDistanceWeightage,
        BeamDIPC.availabilityTimeWeightage = availabilityTimeWeightage,
        BeamDIPC.availabilityTimeWindowOption = availabilityTimeWindowOption,
        BeamDIPC.acceptanceRatioWeightage = acceptanceRatioWeightage,
        BeamDIPC.acceptanceRatioWindowOption = acceptanceRatioWindowOption,
        BeamDIPC.cancellationRatioWeightage = cancellationRatioWeightage,
        BeamDIPC.cancellationRatioWindowOption = cancellationRatioWindowOption,
        BeamDIPC.minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
        BeamDIPC.minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
        BeamDIPC.intelligentPoolPercentage = intelligentPoolPercentage,
        BeamDIPC.speedNormalizer = speedNormalizer,
        BeamDIPC.driverSpeedWeightage = driverSpeedWeightage,
        BeamDIPC.minLocationUpdates = minLocationUpdates,
        BeamDIPC.locationUpdateSampleTime = locationUpdateSampleTime,
        BeamDIPC.defaultDriverSpeed = defaultDriverSpeed,
        BeamDIPC.createdAt = createdAt,
        BeamDIPC.updatedAt = updatedAt
      }
