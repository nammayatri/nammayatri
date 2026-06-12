{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.Discount where

import qualified Domain.Types.Discount
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Discount as Queries

findAllEnabledByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m ([Domain.Types.Discount.Discount]))
findAllEnabledByMerchantOpCityId merchantOperatingCityId enabled = do
  (Hedis.safeGet $ "driverOfferCachedQueries:Discount:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Enabled-" <> show enabled)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("driverOfferCachedQueries:Discount:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Enabled-" <> show enabled) dataToBeCached expTime
              )
                /=<< Queries.findAllEnabledByMerchantOpCityId merchantOperatingCityId enabled
        )

findByMerchantOpCityIdAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> m ([Domain.Types.Discount.Discount]))
findByMerchantOpCityIdAndType merchantOperatingCityId discountType enabled = do
  (Hedis.safeGet $ "driverOfferCachedQueries:Discount:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":DiscountType-" <> show discountType <> ":Enabled-" <> show enabled)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("driverOfferCachedQueries:Discount:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":DiscountType-" <> show discountType <> ":Enabled-" <> show enabled) dataToBeCached expTime
              )
                /=<< Queries.findByMerchantOpCityIdAndType merchantOperatingCityId discountType enabled
        )
