{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AssetRelease where

import qualified Domain.Types.AssetRelease
import qualified Domain.Types.Extra.AssetRelease
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AssetRelease as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AssetRelease.AssetRelease -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AssetRelease.AssetRelease] -> m ())
createMany = traverse_ create

findLatestByAssetTypeAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Domain.Types.Extra.AssetRelease.AssetType -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.AssetRelease.AssetRelease])
findLatestByAssetTypeAndCity limit offset assetType merchantId merchantOperatingCityId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.assetType $ Se.Eq assetType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

updateRolledBackAt :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.AssetRelease.AssetRelease -> m ())
updateRolledBackAt rolledBackAt id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.rolledBackAt rolledBackAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AssetRelease.AssetRelease -> m (Maybe Domain.Types.AssetRelease.AssetRelease))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AssetRelease.AssetRelease -> m ())
updateByPrimaryKey (Domain.Types.AssetRelease.AssetRelease {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.assetType assetType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rolledBackAt rolledBackAt,
      Se.Set Beam.sha256 sha256,
      Se.Set Beam.sizeBytes sizeBytes,
      Se.Set Beam.sourceRef sourceRef,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.url url,
      Se.Set Beam.version version
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AssetRelease Domain.Types.AssetRelease.AssetRelease where
  fromTType' (Beam.AssetReleaseT {..}) = do
    pure $
      Just
        Domain.Types.AssetRelease.AssetRelease
          { assetType = assetType,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rolledBackAt = rolledBackAt,
            sha256 = sha256,
            sizeBytes = sizeBytes,
            sourceRef = sourceRef,
            updatedAt = updatedAt,
            url = url,
            version = version
          }

instance ToTType' Beam.AssetRelease Domain.Types.AssetRelease.AssetRelease where
  toTType' (Domain.Types.AssetRelease.AssetRelease {..}) = do
    Beam.AssetReleaseT
      { Beam.assetType = assetType,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rolledBackAt = rolledBackAt,
        Beam.sha256 = sha256,
        Beam.sizeBytes = sizeBytes,
        Beam.sourceRef = sourceRef,
        Beam.updatedAt = updatedAt,
        Beam.url = url,
        Beam.version = version
      }
