module Storage.CachedQueries.BusinessHour
  ( findById,
    findAllByIds,
    findAllByPlaceId,
    clearCacheById,
  )
where

import Domain.Types.BusinessHour
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BusinessHour as Queries

findById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id BusinessHour -> m (Maybe BusinessHour)
findById bhId =
  Hedis.safeGet (makeIdKey bhId) >>= \case
    Just a -> return $ Just a
    Nothing -> flip whenJust cacheBusinessHour /=<< Queries.findById bhId

findAllByIds :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => [Id BusinessHour] -> m [BusinessHour]
findAllByIds ids = do
  cachedResults <- mapM (\bhId -> Hedis.safeGet (makeIdKey bhId)) ids
  let (hits, missIds) = partitionCacheResults ids cachedResults
  if null missIds
    then return hits
    else do
      dbResults <- Queries.findAllByIds missIds
      mapM_ cacheBusinessHour dbResults
      return $ hits <> dbResults

findAllByPlaceId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe Text -> m [BusinessHour]
findAllByPlaceId Nothing = Queries.findAllByPlaceId Nothing
findAllByPlaceId (Just pid) =
  Hedis.safeGet (makePlaceIdKey pid) >>= \case
    Just a -> return a
    Nothing -> cachePlaceResults pid /=<< Queries.findAllByPlaceId (Just pid)

clearCacheById :: (CacheFlow m r) => Id BusinessHour -> m ()
clearCacheById bhId = Hedis.del (makeIdKey bhId)

-- internal helpers

cacheBusinessHour :: (CacheFlow m r) => BusinessHour -> m ()
cacheBusinessHour bh = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeIdKey bh.id) bh expTime

cachePlaceResults :: (CacheFlow m r) => Text -> [BusinessHour] -> m ()
cachePlaceResults pid bhs = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makePlaceIdKey pid) bhs expTime
  mapM_ cacheBusinessHour bhs

partitionCacheResults :: [Id BusinessHour] -> [Maybe BusinessHour] -> ([BusinessHour], [Id BusinessHour])
partitionCacheResults ids cachedResults =
  foldr
    ( \(bhId, mbResult) (hits, misses) -> case mbResult of
        Just bh -> (bh : hits, misses)
        Nothing -> (hits, bhId : misses)
    )
    ([], [])
    (zip ids cachedResults)

makeIdKey :: Id BusinessHour -> Text
makeIdKey bhId = "CachedQueries:BusinessHour:Id-" <> bhId.getId

makePlaceIdKey :: Text -> Text
makePlaceIdKey placeId = "CachedQueries:BusinessHour:PlaceId-" <> placeId
