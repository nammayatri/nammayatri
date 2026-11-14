module Storage.CachedQueries.PassCategory
  ( findById,
    findAllByMerchantOperatingCityId,
    clearCache,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassCategory as Domain
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Queries.PassCategoryExtra as Queries

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Domain.PassCategory ->
  m (Maybe Domain.PassCategory)
findById passCategoryId = do
  let cacheKey = makePassCategoryIdKey passCategoryId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just val -> return val
      Nothing -> do
        val <- Queries.findById passCategoryId
        whenJust val $ \v -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey v expTime
        return val

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [Domain.PassCategory]
findAllByMerchantOperatingCityId merchantOperatingCityId = do
  let cacheKey = makeMerchantOperatingCityIdKey merchantOperatingCityId
  IM.withInMemCache [cacheKey] 3600 $ do
    Hedis.safeGet cacheKey >>= \case
      Just a | not (null a) -> pure a
      _ -> do
        categories <- Queries.findAllByMerchantOperatingCityId merchantOperatingCityId
        unless (null categories) $ do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp cacheKey categories expTime
        pure categories

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id Domain.PassCategory -> Id DMOC.MerchantOperatingCity -> m ()
clearCache passCategoryId mocId = do
  Hedis.runInMultiCloudRedisWrite $ do
    Hedis.del (makePassCategoryIdKey passCategoryId)
    Hedis.del (makeMerchantOperatingCityIdKey mocId)
    invalidateConfigInMem LYT.PassCategory
  IM.refreshInMem "PassCategory"

makePassCategoryIdKey :: Id Domain.PassCategory -> Text
makePassCategoryIdKey passCategoryId = "CachedQueries:PassCategory:Id-" <> getId passCategoryId

makeMerchantOperatingCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeMerchantOperatingCityIdKey mocId = "CachedQueries:PassCategory:MerchantOperatingCityId-" <> getId mocId
