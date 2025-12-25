{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchReqLocation where

import qualified Domain.Types.SearchReqLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchReqLocation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchReqLocation.SearchReqLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchReqLocation.SearchReqLocation] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation -> m (Maybe Domain.Types.SearchReqLocation.SearchReqLocation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchReqLocation.SearchReqLocation -> m (Maybe Domain.Types.SearchReqLocation.SearchReqLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchReqLocation.SearchReqLocation -> m ())
updateByPrimaryKey (Domain.Types.SearchReqLocation.SearchReqLocation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.area area,
      Se.Set Beam.areaCode areaCode,
      Se.Set Beam.building building,
      Se.Set Beam.city city,
      Se.Set Beam.country country,
      Se.Set Beam.door door,
      Se.Set Beam.extras extras,
      Se.Set Beam.full_address full_address,
      Se.Set Beam.instructions instructions,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.state state,
      Se.Set Beam.street street,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SearchReqLocation Domain.Types.SearchReqLocation.SearchReqLocation where
  fromTType' (Beam.SearchReqLocationT {..}) = do
    pure $
      Just
        Domain.Types.SearchReqLocation.SearchReqLocation
          { area = area,
            areaCode = areaCode,
            building = building,
            city = city,
            country = country,
            createdAt = createdAt,
            door = door,
            extras = extras,
            full_address = full_address,
            id = Kernel.Types.Id.Id id,
            instructions = instructions,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            state = state,
            street = street,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SearchReqLocation Domain.Types.SearchReqLocation.SearchReqLocation where
  toTType' (Domain.Types.SearchReqLocation.SearchReqLocation {..}) = do
    Beam.SearchReqLocationT
      { Beam.area = area,
        Beam.areaCode = areaCode,
        Beam.building = building,
        Beam.city = city,
        Beam.country = country,
        Beam.createdAt = createdAt,
        Beam.door = door,
        Beam.extras = extras,
        Beam.full_address = full_address,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.instructions = instructions,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.state = state,
        Beam.street = street,
        Beam.updatedAt = updatedAt
      }
