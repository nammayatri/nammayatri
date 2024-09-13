module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogic where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Types.TimeBound
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogic as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic

findByMerchantOpCityAndDomain ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m ([Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic])
findByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:AppDynamicLogic:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId merchantOperatingCityId) <> ":Domain-" <> show domain)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:AppDynamicLogic:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId merchantOperatingCityId) <> ":Domain-" <> show domain) dataToBeCached expTime
              )
                /=<< Queries.findByMerchantOpCityAndDomain Nothing Nothing merchantOperatingCityId domain
        )

delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> TimeBound -> m ()
delete = Queries.delete

clearCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
clearCache cityId domain =
  Hedis.withCrossAppRedis $ Hedis.del $ "driverOfferCachedQueries:AppDynamicLogic:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId cityId) <> ":Domain-" <> show domain

createMany :: (BeamFlow.BeamFlow m r) => [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic] -> m ()
createMany = Queries.createMany
