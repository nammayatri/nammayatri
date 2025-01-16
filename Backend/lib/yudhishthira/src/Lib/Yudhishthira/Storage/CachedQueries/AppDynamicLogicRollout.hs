module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout

findByMerchantOpCityAndDomain ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout]
findByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId merchantOperatingCityId) <> ":Domain-" <> show domain)
    >>= ( \case
            Just a -> if null a then fetchAndCase else pure a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId merchantOperatingCityId) <> ":Domain-" <> show domain) dataToBeCached expTime
      )
        /=<< Queries.findByMerchantOpCityAndDomain merchantOperatingCityId domain

delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
delete = Queries.delete

clearCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
clearCache cityId domain =
  Hedis.withCrossAppRedis $ Hedis.del $ "driverOfferCachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId cityId) <> ":Domain-" <> show domain

createMany :: BeamFlow.BeamFlow m r => [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout] -> m ()
createMany = Queries.createMany
