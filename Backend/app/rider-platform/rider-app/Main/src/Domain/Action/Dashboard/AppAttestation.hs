{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.AppAttestation
  ( getAppAttestationConfig,
    postAppAttestationConfig,
    postAppAttestationIosKeyRevoke,
  )
where

import qualified AppAttestation.Admin as Admin
import qualified AppAttestation.Domain.Types.AppAttestationConfig as Fields
import qualified AppAttestation.Domain.Types.Common as AppAttestation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.RiderConfig as Fields
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfigFromDB)
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.AppAttestation ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.ConfigPilot.Config.MerchantServiceConfig as CPMSC
import qualified Storage.Queries.RiderConfig as QRC
import Storage.Queries.Transformers.MerchantServiceConfig (getServiceNameConfigJson)
import Tools.Error

getAppAttestationConfig :: ShortId DM.Merchant -> Context.City -> Flow Admin.AttestationConfigRes
getAppAttestationConfig merchantShortId opCity = do
  (merchant, merchantOpCity) <- findMerchantAndCity merchantShortId opCity
  Admin.toConfigRes <$> loadSettings merchant.id merchantOpCity.id

postAppAttestationConfig :: ShortId DM.Merchant -> Context.City -> Admin.AttestationConfigUpdateReq -> Flow Admin.AttestationConfigRes
postAppAttestationConfig merchantShortId opCity req = do
  (merchant, merchantOpCity) <- findMerchantAndCity merchantShortId opCity
  current <- loadSettings merchant.id merchantOpCity.id
  update <- Admin.applyConfigUpdate req current
  whenJust update.changedPlayIntegrityConfig $ saveServiceConfig merchant.id merchantOpCity.id . DMSC.AppAttestationServiceConfig
  whenJust update.changedCaptchaConfig $ saveServiceConfig merchant.id merchantOpCity.id . DMSC.CaptchaServiceConfig
  QRC.updateAppAttestationConfig update.settings.config merchantOpCity.id
  CQRC.clearCache merchantOpCity.id
  invalidateConfigInMem LYT.RiderConfig
  logInfo $ "App attestation config updated for " <> merchantOpCity.id.getId <> ": mode " <> maybe "OFF" (show . (.mode)) update.settings.config
  pure $ Admin.toConfigRes update.settings

postAppAttestationIosKeyRevoke :: ShortId DM.Merchant -> Context.City -> Admin.RevokeIosKeyReq -> Flow APISuccess
postAppAttestationIosKeyRevoke merchantShortId opCity req = do
  (_, merchantOpCity) <- findMerchantAndCity merchantShortId opCity
  Admin.revokeIosKey (cast merchantOpCity.id) req

loadSettings :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow Admin.AttestationSettings
loadSettings merchantId merchantOpCityId = do
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  playIntegrityConfig <-
    findServiceConfig (DMSC.AppAttestationService AppAttestation.PlayIntegrity) <&> \case
      Just (DMSC.AppAttestationServiceConfig config) -> Just config
      _ -> Nothing
  captchaConfig <-
    findServiceConfig (DMSC.CaptchaService AppAttestation.SiteVerify) <&> \case
      Just (DMSC.CaptchaServiceConfig config) -> Just config
      _ -> Nothing
  pure Admin.AttestationSettings {config = riderConfig.appAttestationConfig, ..}
  where
    findServiceConfig serviceName = fmap (.serviceConfig) <$> CQMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId serviceName

saveServiceConfig :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceConfig -> Flow ()
saveServiceConfig merchantId merchantOperatingCityId serviceConfig = do
  now <- getCurrentTime
  CQMSC.upsertMerchantServiceConfig
    DMSC.MerchantServiceConfig
      { merchantId,
        merchantOperatingCityId,
        serviceConfig,
        runInCloud = Nothing,
        createdAt = now,
        updatedAt = now
      }
  invalidateConfigInMem LYT.MerchantServiceConfig
  Admin.refreshWhenStored now readStoredUpdatedAt clearServiceConfigCaches
  where
    svcName = fst $ getServiceNameConfigJson serviceConfig
    clearServiceConfigCaches = do
      CQMSC.clearCache merchantId merchantOperatingCityId svcName
      invalidateConfigInMem LYT.MerchantServiceConfig
    readStoredUpdatedAt = do
      rows <-
        getConfigFromDB
          CPMSC.MerchantServiceConfigDimensions
            { CPMSC.merchantOperatingCityId = merchantOperatingCityId.getId,
              CPMSC.merchantId = merchantId.getId,
              CPMSC.serviceName = Just svcName
            }
      pure $ listToMaybe [row.updatedAt | row <- rows, fst (getServiceNameConfigJson row.serviceConfig) == svcName]

findMerchantAndCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
findMerchantAndCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchant.id opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure (merchant, merchantOpCity)
