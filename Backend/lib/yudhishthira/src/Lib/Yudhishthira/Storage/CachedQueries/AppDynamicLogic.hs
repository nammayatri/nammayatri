{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogic where

import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Yudhishthira.Storage.Queries.AppDynamicLogic as Queries
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic

findByMerchantOpCityAndDomain ::
  (BeamFlow.BeamFlow m r) =>
  Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Data.Text.Text ->
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
