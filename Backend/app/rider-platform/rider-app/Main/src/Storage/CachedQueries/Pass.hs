module Storage.CachedQueries.Pass
  ( findById,
    findAllByPassTypeIdAndEnabled,
    clearCache,
  )
where

import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PassExtra as QPass

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPass.Pass ->
  m (Maybe DPass.Pass)
findById passId = do
  let cacheKey = makePassIdKey passId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just val -> return val
      Nothing -> do
        val <- QPass.findById passId
        whenJust val $ \v -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey v expTime
        return val

findAllByPassTypeIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassType.PassType ->
  Bool ->
  m [DPass.Pass]
findAllByPassTypeIdAndEnabled passTypeId enabled = do
  let cacheKey = makePassTypeIdAndEnabledKey passTypeId enabled
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just a | not (null a) -> pure a
      _ -> do
        passes <- QPass.findAllByPassTypeIdAndEnabled passTypeId enabled
        unless (null passes) $ do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey passes expTime
        pure passes

clearCache :: (CacheFlow m r, MonadFlow m) => Id DPass.Pass -> [Id DPassType.PassType] -> m ()
clearCache passId passTypeIds = do
  Hedis.runInMultiCloudRedisWrite $ do
    Hedis.del (makePassIdKey passId)
    forM_ passTypeIds $ \passTypeId ->
      forM_ [True, False] $ Hedis.del . makePassTypeIdAndEnabledKey passTypeId
  IM.refreshInMem passCacheKeyPrefix

passCacheKeyPrefix :: Text
passCacheKeyPrefix = "CachedQueries:Pass:"

makePassIdKey :: Id DPass.Pass -> Text
makePassIdKey passId = passCacheKeyPrefix <> "Id-" <> passId.getId

makePassTypeIdAndEnabledKey :: Id DPassType.PassType -> Bool -> Text
makePassTypeIdAndEnabledKey passTypeId enabled =
  passCacheKeyPrefix <> "PassTypeId-" <> passTypeId.getId <> ":Enabled-" <> show enabled
