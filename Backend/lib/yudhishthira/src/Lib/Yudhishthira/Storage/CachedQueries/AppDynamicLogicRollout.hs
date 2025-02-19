module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRollout as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout
import Sequelize as Se

fetchAllConfigsByMerchantOpCityId ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  m [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout]
fetchAllConfigsByMerchantOpCityId merchantOperatingCityId = do
  Hedis.safeGet (cityConfigsCacheKey merchantOperatingCityId)
    >>= ( \case
            Just a -> if null a then fetchAndCase else pure a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \allRollouts -> do
          let configRollouts = filter isConfig allRollouts
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (cityConfigsCacheKey merchantOperatingCityId) configRollouts expTime
      )
        /=<< Queries.findAllByMerchantOpCityId merchantOperatingCityId

    isConfig :: Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout -> Bool
    isConfig rollout =
      case rollout.domain of
        Lib.Yudhishthira.Types.DRIVER_CONFIG _ -> True
        Lib.Yudhishthira.Types.RIDER_CONFIG _ -> True
        _ -> False

findBaseRolloutByMerchantOpCityAndDomain ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout)
findBaseRolloutByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  Hedis.safeGet (baseRolloutCacheKey merchantOperatingCityId domain)
    >>= ( \case
            Just a -> pure $ Just a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (baseRolloutCacheKey merchantOperatingCityId domain) dataToBeCached expTime
      )
        /=<< listToMaybe
          <$> findAllWithKV
            [ Se.And
                [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
                  Se.Is Beam.domain $ Se.Eq domain,
                  Se.Is Beam.isBaseVersion $ Se.Eq (Just True)
                ]
            ]

findByMerchantOpCityAndDomain ::
  BeamFlow.BeamFlow m r =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.LogicDomain ->
  m [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout]
findByMerchantOpCityAndDomain merchantOperatingCityId domain =
  do
    Hedis.safeGet $ domainCacheKey merchantOperatingCityId domain
    >>= ( \case
            Just a -> if null a then fetchAndCase else pure a
            Nothing -> fetchAndCase
        )
  where
    fetchAndCase =
      ( \dataToBeCached -> do
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          Hedis.setExp (domainCacheKey merchantOperatingCityId domain) dataToBeCached expTime
      )
        /=<< Queries.findByMerchantOpCityAndDomain merchantOperatingCityId domain

delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
delete = Queries.delete

clearCityConfigsCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> m ()
clearCityConfigsCache cityId = Hedis.del $ cityConfigsCacheKey cityId

clearDomainCache :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
clearDomainCache cityId domain = Hedis.del $ domainCacheKey cityId domain

createMany :: BeamFlow.BeamFlow m r => [Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout] -> m ()
createMany = Queries.createMany

cityConfigsCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Text
cityConfigsCacheKey cityId = "CachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId cityId

domainCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> Text
domainCacheKey cityId domain = "CachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId cityId <> ":Domain-" <> show domain

baseRolloutCacheKey :: Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> Text
baseRolloutCacheKey cityId domain = "CachedQueries:AppDynamicLogicRollout:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId cityId <> ":Domain-" <> show domain <> ":BaseRollout"
