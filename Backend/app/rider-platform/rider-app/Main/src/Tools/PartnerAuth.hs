{-# OPTIONS_GHC -Wno-deprecations #-}

module Tools.PartnerAuth
  ( getPartnerAuthConfig,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified PartnerAuth.Interface.Types as PartnerAuth
import qualified PartnerAuth.Types as PartnerAuth
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

-- | Fetch the active partner-auth provider config (DB-backed MerchantServiceConfig)
-- for the given merchant/op-city and provider.
--
-- Uses the EXACT (merchant, op-city, serviceName) lookup rather than ConfigPilot's
-- getOneConfig, so no city-level fallback can select a different city's credentials
-- for partner auth. Returns Nothing if no row exists or the stored config is of an
-- unexpected service type.
getPartnerAuthConfig ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  PartnerAuth.PartnerAuthService ->
  m (Maybe PartnerAuth.PartnerAuthServiceConfig)
getPartnerAuthConfig merchantId merchantOperatingCityId provider = do
  mbConfig <- CQMSC.findByMerchantOpCityIdAndService merchantId merchantOperatingCityId (DMSC.PartnerAuthService provider)
  pure $ case mbConfig of
    Just config -> case config.serviceConfig of
      DMSC.PartnerAuthServiceConfig msc -> Just msc
      _ -> Nothing
    Nothing -> Nothing
