module Storage.CachedQueries.ServiceCategory
  ( findById,
    findAllByIds,
    findAllByPlaceId,
    clearCacheById,
    clearCacheByPlaceId,
  )
where

import Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.ServiceCategory as Queries

findById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id ServiceCategory -> m (Maybe ServiceCategory)
findById scId =
  Hedis.safeGet (makeIdKey scId) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheServiceCategory /=<< Queries.findById scId

findAllByIds :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => [Id ServiceCategory] -> m [ServiceCategory]
findAllByIds ids = do
  cachedResults <- mapM (Hedis.safeGet . makeIdKey) ids
  let (hits, missIds) = partitionCacheResults ids cachedResults
  if null missIds
    then return hits
    else do
      dbResults <- catMaybes <$> mapM Queries.findById missIds
      mapM_ cacheServiceCategory dbResults
      return $ hits <> dbResults

findAllByPlaceId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> m [ServiceCategory]
findAllByPlaceId Nothing = Queries.findAllByPlaceId Nothing
findAllByPlaceId (Just placeId) =
  Hedis.safeGet (makePlaceIdKey placeId) >>= \case
    Just a -> return a
    Nothing -> cachePlaceResults placeId /=<< Queries.findAllByPlaceId (Just placeId)

clearCacheById :: (CacheFlow m r) => Id ServiceCategory -> m ()
clearCacheById scId = Hedis.del (makeIdKey scId)

clearCacheByPlaceId :: (CacheFlow m r) => Maybe Text -> m ()
clearCacheByPlaceId Nothing = return ()
clearCacheByPlaceId (Just placeId) = Hedis.del (makePlaceIdKey placeId)

-- internal helpers

cacheServiceCategory :: (CacheFlow m r) => ServiceCategory -> m ()
cacheServiceCategory sc = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeIdKey sc.id) sc expTime

cachePlaceResults :: (CacheFlow m r) => Text -> [ServiceCategory] -> m ()
cachePlaceResults placeId scs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makePlaceIdKey placeId) scs expTime
  mapM_ cacheServiceCategory scs

partitionCacheResults :: [Id ServiceCategory] -> [Maybe ServiceCategory] -> ([ServiceCategory], [Id ServiceCategory])
partitionCacheResults ids cachedResults =
  foldr
    ( \(scId, mbResult) (hits, misses) -> case mbResult of
        Just sc -> (sc : hits, misses)
        Nothing -> (hits, scId : misses)
    )
    ([], [])
    (zip ids cachedResults)

makeIdKey :: Id ServiceCategory -> Text
makeIdKey scId = "CachedQueries:ServiceCategory:Id-" <> scId.getId

makePlaceIdKey :: Text -> Text
makePlaceIdKey placeId = "CachedQueries:ServiceCategory:PlaceId-" <> placeId
