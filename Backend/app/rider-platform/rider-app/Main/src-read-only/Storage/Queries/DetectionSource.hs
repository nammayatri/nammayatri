{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DetectionSource where

import qualified Data.Aeson
import qualified Domain.Types.DetectionSource
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DetectionSource as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DetectionSource.DetectionSource -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DetectionSource.DetectionSource] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.DetectionSource.DetectionSource])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DetectionSource.DetectionSource -> m (Maybe Domain.Types.DetectionSource.DetectionSource))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DetectionSource.DetectionSource -> m (Maybe Domain.Types.DetectionSource.DetectionSource))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DetectionSource.DetectionSource -> m ())
updateByPrimaryKey (Domain.Types.DetectionSource.DetectionSource {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.anomalyAlertsEnabled anomalyAlertsEnabled,
      Se.Set Beam.authCode authCode,
      Se.Set Beam.authMode authMode,
      Se.Set Beam.authSecret (authSecret <&> unEncrypted),
      Se.Set Beam.enabled enabled,
      Se.Set Beam.enforcementEnabled enforcementEnabled,
      Se.Set Beam.enforcementGraceMinutes enforcementGraceMinutes,
      Se.Set Beam.imagesEnabled imagesEnabled,
      Se.Set Beam.mappings (Data.Aeson.toJSON mappings),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.occupancyMode occupancyMode,
      Se.Set Beam.vendorKey vendorKey,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DetectionSource Domain.Types.DetectionSource.DetectionSource where
  fromTType' (Beam.DetectionSourceT {..}) = do
    pure $
      Just
        Domain.Types.DetectionSource.DetectionSource
          { anomalyAlertsEnabled = anomalyAlertsEnabled,
            authCode = authCode,
            authMode = authMode,
            authSecret = Encrypted <$> authSecret,
            enabled = enabled,
            enforcementEnabled = enforcementEnabled,
            enforcementGraceMinutes = enforcementGraceMinutes,
            id = Kernel.Types.Id.Id id,
            imagesEnabled = imagesEnabled,
            mappings = (\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> x; Data.Aeson.Error _ -> []) mappings,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            occupancyMode = occupancyMode,
            vendorKey = vendorKey,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DetectionSource Domain.Types.DetectionSource.DetectionSource where
  toTType' (Domain.Types.DetectionSource.DetectionSource {..}) = do
    Beam.DetectionSourceT
      { Beam.anomalyAlertsEnabled = anomalyAlertsEnabled,
        Beam.authCode = authCode,
        Beam.authMode = authMode,
        Beam.authSecret = authSecret <&> unEncrypted,
        Beam.enabled = enabled,
        Beam.enforcementEnabled = enforcementEnabled,
        Beam.enforcementGraceMinutes = enforcementGraceMinutes,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.imagesEnabled = imagesEnabled,
        Beam.mappings = Data.Aeson.toJSON mappings,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.occupancyMode = occupancyMode,
        Beam.vendorKey = vendorKey,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
