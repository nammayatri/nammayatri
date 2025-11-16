{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlaceNameCache where

import qualified Domain.Types.PlaceNameCache
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PlaceNameCache as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlaceNameCache.PlaceNameCache -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PlaceNameCache.PlaceNameCache] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findPlaceByGeoHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.PlaceNameCache.PlaceNameCache])
findPlaceByGeoHash geoHash = do findAllWithKV [Se.Is Beam.geoHash $ Se.Eq geoHash]

findPlaceByPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.PlaceNameCache.PlaceNameCache])
findPlaceByPlaceId placeId = do findAllWithKV [Se.Is Beam.placeId $ Se.Eq placeId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache -> m (Maybe Domain.Types.PlaceNameCache.PlaceNameCache))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlaceNameCache.PlaceNameCache -> m ())
updateByPrimaryKey (Domain.Types.PlaceNameCache.PlaceNameCache {..}) = do
  updateWithKV
    [ Se.Set Beam.addressComponents addressComponents,
      Se.Set Beam.formattedAddress formattedAddress,
      Se.Set Beam.geoHash geoHash,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.placeId placeId,
      Se.Set Beam.plusCode plusCode
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PlaceNameCache Domain.Types.PlaceNameCache.PlaceNameCache where
  fromTType' (Beam.PlaceNameCacheT {..}) = do
    pure $
      Just
        Domain.Types.PlaceNameCache.PlaceNameCache
          { addressComponents = addressComponents,
            createdAt = createdAt,
            formattedAddress = formattedAddress,
            geoHash = geoHash,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            placeId = placeId,
            plusCode = plusCode
          }

instance ToTType' Beam.PlaceNameCache Domain.Types.PlaceNameCache.PlaceNameCache where
  toTType' (Domain.Types.PlaceNameCache.PlaceNameCache {..}) = do
    Beam.PlaceNameCacheT
      { Beam.addressComponents = addressComponents,
        Beam.createdAt = createdAt,
        Beam.formattedAddress = formattedAddress,
        Beam.geoHash = geoHash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.placeId = placeId,
        Beam.plusCode = plusCode
      }
