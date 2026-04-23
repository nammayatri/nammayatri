module Storage.CachedQueries.Pass
  ( findAllByPassTypeIdAndEnabled,
    clearCacheByPassTypeIdAndEnabled,
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

clearCacheByPassTypeIdAndEnabled :: (CacheFlow m r) => Id DPassType.PassType -> Bool -> m ()
clearCacheByPassTypeIdAndEnabled passTypeId enabled =
  Hedis.del (makePassTypeIdAndEnabledKey passTypeId enabled)

makePassTypeIdAndEnabledKey :: Id DPassType.PassType -> Bool -> Text
makePassTypeIdAndEnabledKey passTypeId enabled =
  "CachedQueries:Pass:PassTypeId-" <> passTypeId.getId <> ":Enabled-" <> show enabled
