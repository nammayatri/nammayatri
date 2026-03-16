module Storage.CachedQueries.Geometry
  ( findGeometryByStateAndCity,
    clearCacheByStateAndCity,
  )
where

import Domain.Types.Geometry
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import qualified Storage.Queries.Geometry as Queries

findGeometryByStateAndCity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Context.City -> Context.IndianState -> m (Maybe Geometry)
findGeometryByStateAndCity city state =
  Hedis.safeGet (makeStateAndCityKey city state) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust (cacheGeometry city state) /=<< Queries.findGeometryByStateAndCity city state

clearCacheByStateAndCity :: (CacheFlow m r) => Context.City -> Context.IndianState -> m ()
clearCacheByStateAndCity city state = Hedis.del (makeStateAndCityKey city state)

-- internal helpers

cacheGeometry :: (CacheFlow m r) => Context.City -> Context.IndianState -> Geometry -> m ()
cacheGeometry city state _geo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeStateAndCityKey city state) _geo expTime

makeStateAndCityKey :: Context.City -> Context.IndianState -> Text
makeStateAndCityKey city state = "CachedQueries:Geometry:City-" <> show city <> ":State-" <> show state
