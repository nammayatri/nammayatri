{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DomainDiscountConfig where

import Domain.Types.Common (ServiceTierType)
import Domain.Types.DomainDiscountConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified SharedLogic.Type as SLT
import qualified Storage.Queries.DomainDiscountConfig as Queries

findByMerchantOpCityIdAndDomainAndBillingCategory ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  SLT.BillingCategory ->
  ServiceTierType ->
  m (Maybe DomainDiscountConfig)
findByMerchantOpCityIdAndDomainAndBillingCategory merchantOpCityId domain billingCategory vehicleServiceTier = do
  Hedis.safeGet (makeDomainDiscountKey merchantOpCityId domain billingCategory vehicleServiceTier) >>= \case
    Just cfg -> return cfg
    Nothing -> do
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      cfg <- Queries.findByMerchantOpCityIdAndDomainAndBillingCategory merchantOpCityId domain billingCategory vehicleServiceTier
      whenJust cfg $ \c -> Hedis.setExp (makeDomainDiscountKey merchantOpCityId domain billingCategory vehicleServiceTier) c expTime
      return cfg

-- | Resolve the domain discount percentage for a given email domain.
-- Looks up DomainDiscountConfig; if found and enabled, returns the percentage.
-- Falls back to wildcard domain "*" if no exact match is found.
resolveDomainDiscountPercentage ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Text ->
  SLT.BillingCategory ->
  ServiceTierType ->
  m (Maybe Double)
resolveDomainDiscountPercentage _ Nothing _ _ = return Nothing
resolveDomainDiscountPercentage merchantOpCityId (Just domain) billingCategory vehicleServiceTier = do
  mbConfig <- findByMerchantOpCityIdAndDomainAndBillingCategory merchantOpCityId domain billingCategory vehicleServiceTier
  case mbConfig of
    Just config | config.enabled -> return $ Just config.discountPercentage
    _ -> do
      -- Fallback to wildcard domain "*"
      mbWildcard <- findByMerchantOpCityIdAndDomainAndBillingCategory merchantOpCityId "*" billingCategory vehicleServiceTier
      case mbWildcard of
        Just wc | wc.enabled -> return $ Just wc.discountPercentage
        _ -> return Nothing

makeDomainDiscountKey :: Id MerchantOperatingCity -> Text -> SLT.BillingCategory -> ServiceTierType -> Text
makeDomainDiscountKey merchantOpCityId domain billingCategory vehicleServiceTier =
  "driverOffer:CachedQueries:DomainDiscountConfig:MocId-"
    <> merchantOpCityId.getId
    <> ":Domain-"
    <> domain
    <> ":BillingCategory-"
    <> show billingCategory
    <> ":VehicleServiceTier-"
    <> show vehicleServiceTier
