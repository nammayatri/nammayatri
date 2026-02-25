module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicAlwaysOn where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicAlwaysOn as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn

findByMerchantOpCityAndDomainOrdered ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m [Lib.Yudhishthira.Types.AppDynamicLogicAlwaysOn.AppDynamicLogicAlwaysOn]
findByMerchantOpCityAndDomainOrdered merchantOperatingCityId domain = do
  Hedis.safeGet (alwaysOnCacheKey merchantOperatingCityId domain)
    >>= ( \case
            Just a -> if null a then fetchAndCache else pure a
            Nothing -> fetchAndCache
        )
  where
    fetchAndCache = do
      rows <- Queries.findByMerchantOpCityAndDomainOrdered merchantOperatingCityId domain
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (alwaysOnCacheKey merchantOperatingCityId domain) rows expTime
      pure rows

alwaysOnCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> Text
alwaysOnCacheKey cityId domain = "yudhishthira-CachedQueries:AppDynamicLogicAlwaysOn:MerchantOperatingCityId-" <> Kernel.Types.Id.getId cityId <> ":Domain-" <> show domain

clearCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
clearCache cityId domain = Hedis.del $ alwaysOnCacheKey cityId domain
