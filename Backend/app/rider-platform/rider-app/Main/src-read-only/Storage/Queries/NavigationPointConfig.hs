{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NavigationPointConfig where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.NavigationPointConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NavigationPointConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NavigationPointConfig.NavigationPointConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.NavigationPointConfig.NavigationPointConfig] -> m ())
createMany = traverse_ create

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.NavigationPointConfig.NavigationPointConfig))
findByMerchantOperatingCityId merchantOperatingCityId = do findOneWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.NavigationPointConfig.NavigationPointConfig))
findByPrimaryKey merchantOperatingCityId = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.NavigationPointConfig.NavigationPointConfig -> m ())
updateByPrimaryKey (Domain.Types.NavigationPointConfig.NavigationPointConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bucketGeohashPrecision bucketGeohashPrecision,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.maxPointsToShow maxPointsToShow,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.pointGeohashPrecision pointGeohashPrecision,
      Se.Set Beam.radiusInMeters radiusInMeters,
      Se.Set Beam.requiredFieldMask requiredFieldMask,
      Se.Set Beam.ttlInSeconds ttlInSeconds,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]]

instance FromTType' Beam.NavigationPointConfig Domain.Types.NavigationPointConfig.NavigationPointConfig where
  fromTType' (Beam.NavigationPointConfigT {..}) = do
    pure $
      Just
        Domain.Types.NavigationPointConfig.NavigationPointConfig
          { bucketGeohashPrecision = bucketGeohashPrecision,
            enabled = enabled,
            maxPointsToShow = maxPointsToShow,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pointGeohashPrecision = pointGeohashPrecision,
            radiusInMeters = radiusInMeters,
            requiredFieldMask = requiredFieldMask,
            ttlInSeconds = ttlInSeconds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.NavigationPointConfig Domain.Types.NavigationPointConfig.NavigationPointConfig where
  toTType' (Domain.Types.NavigationPointConfig.NavigationPointConfig {..}) = do
    Beam.NavigationPointConfigT
      { Beam.bucketGeohashPrecision = bucketGeohashPrecision,
        Beam.enabled = enabled,
        Beam.maxPointsToShow = maxPointsToShow,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pointGeohashPrecision = pointGeohashPrecision,
        Beam.radiusInMeters = radiusInMeters,
        Beam.requiredFieldMask = requiredFieldMask,
        Beam.ttlInSeconds = ttlInSeconds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
