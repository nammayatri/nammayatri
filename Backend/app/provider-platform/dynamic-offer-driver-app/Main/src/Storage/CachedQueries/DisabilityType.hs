module Storage.CachedQueries.DisabilityType where

import Domain.Types.DisabilityType
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.DisabilityType as Queries

findAllByLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Language -> m [DisabilityType]
findAllByLanguage language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeDisabilityTypeByLanguageKey language) >>= \case
    Just a -> pure a
    Nothing -> cacheAllDisabilityTypeByLanguage language /=<< Queries.findAllByLanguage language

cacheAllDisabilityTypeByLanguage :: (CacheFlow m r) => Language -> [DisabilityType] -> m ()
cacheAllDisabilityTypeByLanguage language disabilityType = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeDisabilityTypeByLanguageKey language) disabilityType expTime

makeDisabilityTypeByLanguageKey :: Language -> Text
makeDisabilityTypeByLanguageKey language = "driver-offer:CachedQueries:DisabilityType:Language-" <> show language
