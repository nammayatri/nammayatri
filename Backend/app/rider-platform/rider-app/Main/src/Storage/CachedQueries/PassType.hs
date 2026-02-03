module Storage.CachedQueries.PassType where

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

makePassTypeIdKey :: Id Domain.PassType -> Text
makePassTypeIdKey passTypeId = "CachedQueries:PassType:Id-" <> getId passTypeId
