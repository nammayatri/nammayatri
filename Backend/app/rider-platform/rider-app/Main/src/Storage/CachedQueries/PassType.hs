module Storage.CachedQueries.PassType
  ( findById,
    findAllByPassCategoryId,
    clearCache,
  )
where

import qualified Domain.Types.PassCategory as DPassCategory
import qualified Domain.Types.PassType as Domain
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common
import qualified Storage.Queries.PassTypeExtra as Queries

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.PassType ->
  m (Maybe Domain.PassType)
findById passTypeId = do
  let cacheKey = makePassTypeIdKey passTypeId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just val -> return val
      Nothing -> do
        val <- Queries.findById passTypeId
        whenJust val $ \v -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey v expTime
        return val

findAllByPassCategoryId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassCategory.PassCategory ->
  m [Domain.PassType]
findAllByPassCategoryId passCategoryId = do
  let cacheKey = makePassCategoryIdKey passCategoryId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just a | not (null a) -> pure a
      _ -> do
        passTypes <- Queries.findAllByPassCategoryId passCategoryId
        unless (null passTypes) $ do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey passTypes expTime
        pure passTypes

clearCache :: (CacheFlow m r, MonadFlow m) => Id Domain.PassType -> [Id DPassCategory.PassCategory] -> m ()
clearCache passTypeId passCategoryIds = do
  Hedis.runInMultiCloudRedisWrite $ do
    Hedis.del (makePassTypeIdKey passTypeId)
    mapM_ (Hedis.del . makePassCategoryIdKey) passCategoryIds
  IM.refreshInMem passTypeCacheKeyPrefix

passTypeCacheKeyPrefix :: Text
passTypeCacheKeyPrefix = "CachedQueries:PassType:"

makePassTypeIdKey :: Id Domain.PassType -> Text
makePassTypeIdKey passTypeId = passTypeCacheKeyPrefix <> "Id-" <> getId passTypeId

makePassCategoryIdKey :: Id DPassCategory.PassCategory -> Text
makePassCategoryIdKey passCategoryId = passTypeCacheKeyPrefix <> "PassCategoryId-" <> getId passCategoryId
