module Storage.CachedQueries.Pass
  ( findAllByPassTypeIdAndEnabled,
  )
where

import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PassExtra as QPass

findAllByPassTypeIdAndEnabled ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPassType.PassType ->
  Bool ->
  m [DPass.Pass]
findAllByPassTypeIdAndEnabled passTypeId enabled = do
  let key = makePassTypeIdAndEnabledKey passTypeId enabled
  Hedis.safeGet key >>= \case
    Just a -> return a
    Nothing -> do
      passes <- QPass.findAllByPassTypeIdAndEnabled passTypeId enabled
      cachePasses key passes
      return passes

cachePasses :: (CacheFlow m r, MonadFlow m) => Text -> [DPass.Pass] -> m ()
cachePasses key passes = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key passes expTime

makePassTypeIdAndEnabledKey :: Id DPassType.PassType -> Bool -> Text
makePassTypeIdAndEnabledKey passTypeId enabled =
  "CachedQueries:Pass:PassTypeId-" <> passTypeId.getId <> ":Enabled-" <> show enabled
