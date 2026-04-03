{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.PlaceNameCache where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PlaceNameCache
import qualified Storage.Beam.PlaceNameCache as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PlaceNameCache.PlaceNameCache -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PlaceNameCache.PlaceNameCache] -> m ())
createMany = traverse_ create
deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PlaceNameCache.PlaceNameCache -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findPlaceByGeoHash :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.PlaceNameCache.PlaceNameCache]))
findPlaceByGeoHash geoHash = do findAllWithKV [Se.Is Beam.geoHash $ Se.Eq geoHash]
findPlaceByPlaceId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.PlaceNameCache.PlaceNameCache]))
findPlaceByPlaceId placeId = do findAllWithKV [Se.Is Beam.placeId $ Se.Eq placeId]



instance FromTType' Beam.PlaceNameCache Domain.Types.PlaceNameCache.PlaceNameCache
    where fromTType' (Beam.PlaceNameCacheT {..}) = do pure $ Just Domain.Types.PlaceNameCache.PlaceNameCache{addressComponents = addressComponents,
                                                                                                             formattedAddress = formattedAddress,
                                                                                                             geoHash = geoHash,
                                                                                                             id = Kernel.Types.Id.Id id,
                                                                                                             lat = lat,
                                                                                                             lon = lon,
                                                                                                             placeId = placeId,
                                                                                                             plusCode = plusCode,
                                                                                                             createdAt = createdAt}
instance ToTType' Beam.PlaceNameCache Domain.Types.PlaceNameCache.PlaceNameCache
    where toTType' (Domain.Types.PlaceNameCache.PlaceNameCache {..}) = do Beam.PlaceNameCacheT{Beam.addressComponents = addressComponents,
                                                                                               Beam.formattedAddress = formattedAddress,
                                                                                               Beam.geoHash = geoHash,
                                                                                               Beam.id = Kernel.Types.Id.getId id,
                                                                                               Beam.lat = lat,
                                                                                               Beam.lon = lon,
                                                                                               Beam.placeId = placeId,
                                                                                               Beam.plusCode = plusCode,
                                                                                               Beam.createdAt = createdAt}



