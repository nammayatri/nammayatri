{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.GeohashArea where

import qualified Domain.Types.GeohashArea
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.GeohashArea as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GeohashArea.GeohashArea -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.GeohashArea.GeohashArea] -> m ())
createMany = traverse_ create

findAllByMerchantOperatingCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m ([Domain.Types.GeohashArea.GeohashArea]))
findAllByMerchantOperatingCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.GeohashArea.GeohashArea -> m (Maybe Domain.Types.GeohashArea.GeohashArea))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByMerchantOperatingCityAndGeohash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Text -> m (Maybe Domain.Types.GeohashArea.GeohashArea))
findByMerchantOperatingCityAndGeohash merchantOperatingCityId geohash = do findOneWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId), Se.Is Beam.geohash $ Se.Eq geohash]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GeohashArea.GeohashArea -> m ())
updateByPrimaryKey (Domain.Types.GeohashArea.GeohashArea {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.areaName areaName,
      Se.Set Beam.geohash geohash,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.GeohashArea Domain.Types.GeohashArea.GeohashArea where
  fromTType' (Beam.GeohashAreaT {..}) = do
    pure $
      Just
        Domain.Types.GeohashArea.GeohashArea
          { areaName = areaName,
            createdAt = createdAt,
            geohash = geohash,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.GeohashArea Domain.Types.GeohashArea.GeohashArea where
  toTType' (Domain.Types.GeohashArea.GeohashArea {..}) = do
    Beam.GeohashAreaT
      { Beam.areaName = areaName,
        Beam.createdAt = createdAt,
        Beam.geohash = geohash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
