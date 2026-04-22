module Storage.CachedQueries.PassType
  ( findById,
    findAllByPassCategoryId,
    clearCacheById,
    clearCacheByPassCategoryId,
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

clearCacheById :: (CacheFlow m r) => Id Domain.PassType -> m ()
clearCacheById passTypeId = Hedis.del (makePassTypeIdKey passTypeId)

clearCacheByPassCategoryId :: (CacheFlow m r) => Id DPassCategory.PassCategory -> m ()
clearCacheByPassCategoryId passCategoryId = Hedis.del (makePassCategoryIdKey passCategoryId)

makePassTypeIdKey :: Id Domain.PassType -> Text
makePassTypeIdKey passTypeId = "CachedQueries:PassType:Id-" <> getId passTypeId

makePassCategoryIdKey :: Id DPassCategory.PassCategory -> Text
makePassCategoryIdKey passCategoryId = "CachedQueries:PassType:PassCategoryId-" <> getId passCategoryId
