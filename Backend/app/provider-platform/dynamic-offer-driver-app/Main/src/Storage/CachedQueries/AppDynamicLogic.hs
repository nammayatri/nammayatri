{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.AppDynamicLogic where

import qualified Data.Text
import qualified Domain.Types.AppDynamicLogic
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AppDynamicLogic as Queries

findByMerchantOpCityAndDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Data.Text.Text -> m ([Domain.Types.AppDynamicLogic.AppDynamicLogic]))
findByMerchantOpCityAndDomain merchantOperatingCityId domain = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:AppDynamicLogic:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":Domain-" <> show domain)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:AppDynamicLogic:" <> ":MerchantOperatingCityId-" <> show (Kernel.Types.Id.getId <$> merchantOperatingCityId) <> ":Domain-" <> show domain) dataToBeCached expTime
              )
                /=<< Queries.findByMerchantOpCityAndDomain Nothing Nothing merchantOperatingCityId domain
        )
