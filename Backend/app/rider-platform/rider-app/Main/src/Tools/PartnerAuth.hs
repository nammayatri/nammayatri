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
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Interface.Types (getOneConfig)

-- | Fetch the active partner-auth provider config (DB-backed MerchantServiceConfig)
-- for the given merchant/op-city and provider. Returns Nothing if no config row
-- exists or the stored config is of an unexpected service type.
getPartnerAuthConfig ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  PartnerAuth.PartnerAuthService ->
  m (Maybe PartnerAuth.PartnerAuthServiceConfig)
getPartnerAuthConfig merchantId merchantOperatingCityId provider = do
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Just (DMSC.PartnerAuthService provider)
          }
      )
  pure $ case mbConfig of
    Just config -> case config.serviceConfig of
      DMSC.PartnerAuthServiceConfig msc -> Just msc
      _ -> Nothing
    Nothing -> Nothing
