{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Station where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Station as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Station.Station -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Station.Station] -> m ())
createMany = traverse_ create

findAllByBppId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.Station.Station]))
findAllByBppId bppId = do findAllWithKV [Se.Is Beam.bppId $ Se.Eq bppId]

findAllByMerchantOperatingCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.Station.Station]))
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Station.Station))
findByCode code = do findOneWithKV [Se.Is Beam.code $ Se.Eq code]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStationById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Station.Station -> m ())
updateStationById lat lon name id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.lat lat, Se.Set Beam.lon lon, Se.Set Beam.name name, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Station.Station -> m ())
updateByPrimaryKey (Domain.Types.Station.Station {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppId bppId,
      Se.Set Beam.code code,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.merchnatId (Kernel.Types.Id.getId merchnatId),
      Se.Set Beam.name name,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Station Domain.Types.Station.Station where
  fromTType' (Beam.StationT {..}) = do
    pure $
      Just
        Domain.Types.Station.Station
          { bppId = bppId,
            code = code,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            merchnatId = Kernel.Types.Id.Id merchnatId,
            name = name,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Station Domain.Types.Station.Station where
  toTType' (Domain.Types.Station.Station {..}) = do
    Beam.StationT
      { Beam.bppId = bppId,
        Beam.code = code,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.merchnatId = Kernel.Types.Id.getId merchnatId,
        Beam.name = name,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
