{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchDestinationsCache where

import qualified Domain.Types.SearchDestinationsCache
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchDestinationsCache as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchDestinationsCache.SearchDestinationsCache -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SearchDestinationsCache.SearchDestinationsCache] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchDestinationsCache.SearchDestinationsCache -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByGeoHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.SearchDestinationsCache.SearchDestinationsCache]))
findByGeoHash geoHash = do findAllWithKV [Se.Is Beam.geoHash $ Se.Eq geoHash]

instance FromTType' Beam.SearchDestinationsCache Domain.Types.SearchDestinationsCache.SearchDestinationsCache where
  fromTType' (Beam.SearchDestinationsCacheT {..}) = do
    pure $
      Just
        Domain.Types.SearchDestinationsCache.SearchDestinationsCache
          { geoHash = geoHash,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            response = response,
            createdAt = createdAt
          }

instance ToTType' Beam.SearchDestinationsCache Domain.Types.SearchDestinationsCache.SearchDestinationsCache where
  toTType' (Domain.Types.SearchDestinationsCache.SearchDestinationsCache {..}) = do
    Beam.SearchDestinationsCacheT
      { Beam.geoHash = geoHash,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.response = response,
        Beam.createdAt = createdAt
      }
