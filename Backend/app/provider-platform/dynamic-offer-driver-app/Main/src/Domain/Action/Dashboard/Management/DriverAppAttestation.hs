{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.Management.DriverAppAttestation
  ( getDriverAppAttestationConfig,
    postDriverAppAttestationConfig,
    postDriverAppAttestationIosKeyRevoke,
  )
where

import qualified AppAttestation.Admin as Admin
import qualified AppAttestation.Domain.Types.AppAttestationConfig as Fields
import qualified AppAttestation.Domain.Types.Common as AppAttestation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.TransporterConfig as Fields
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
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.ConfigPilot.Config.MerchantServiceConfig as CPMSC
import Storage.Queries.Transformers.MerchantServiceConfig (getServiceName)
import qualified Storage.Queries.TransporterConfig as QTC
import Tools.Error

getDriverAppAttestationConfig :: ShortId DM.Merchant -> Context.City -> Flow Admin.AttestationConfigRes
getDriverAppAttestationConfig merchantShortId opCity = do
  (_, merchantOpCityId) <- findMerchantAndCity merchantShortId opCity
  Admin.toConfigRes <$> loadSettings merchantOpCityId

postDriverAppAttestationConfig :: ShortId DM.Merchant -> Context.City -> Admin.AttestationConfigUpdateReq -> Flow Admin.AttestationConfigRes
postDriverAppAttestationConfig merchantShortId opCity req = do
  (merchant, merchantOpCityId) <- findMerchantAndCity merchantShortId opCity
  current <- loadSettings merchantOpCityId
  update <- Admin.applyConfigUpdate req current
  whenJust update.changedPlayIntegrityConfig $ saveServiceConfig merchant.id merchantOpCityId . DMSC.AppAttestationServiceConfig
  whenJust update.changedCaptchaConfig $ saveServiceConfig merchant.id merchantOpCityId . DMSC.CaptchaServiceConfig
  QTC.updateAppAttestationConfig update.settings.config merchantOpCityId
  CQTC.clearCache merchantOpCityId
  invalidateConfigInMem LYT.TransporterConfig
  logInfo $ "App attestation config updated for " <> merchantOpCityId.getId <> ": mode " <> maybe "OFF" (show . (.mode)) update.settings.config
  pure $ Admin.toConfigRes update.settings

postDriverAppAttestationIosKeyRevoke :: ShortId DM.Merchant -> Context.City -> Admin.RevokeIosKeyReq -> Flow APISuccess
postDriverAppAttestationIosKeyRevoke merchantShortId opCity req = do
  (_, merchantOpCityId) <- findMerchantAndCity merchantShortId opCity
  Admin.revokeIosKey (cast merchantOpCityId) req

loadSettings :: Id DMOC.MerchantOperatingCity -> Flow Admin.AttestationSettings
loadSettings merchantOpCityId = do
  transporterConfig <- QTC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  playIntegrityConfig <-
    findServiceConfig (DMSC.AppAttestationService AppAttestation.PlayIntegrity) <&> \case
      Just (DMSC.AppAttestationServiceConfig config) -> Just config
      _ -> Nothing
  captchaConfig <-
    findServiceConfig (DMSC.CaptchaService AppAttestation.SiteVerify) <&> \case
      Just (DMSC.CaptchaServiceConfig config) -> Just config
      _ -> Nothing
  pure Admin.AttestationSettings {config = transporterConfig.appAttestationConfig, ..}
  where
    findServiceConfig serviceName = fmap (.serviceConfig) <$> CQMSC.findByServiceAndCity serviceName merchantOpCityId

saveServiceConfig :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceConfig -> Flow ()
saveServiceConfig merchantId merchantOpCityId serviceConfig = do
  now <- getCurrentTime
  CQMSC.upsertMerchantServiceConfig
    DMSC.MerchantServiceConfig
      { merchantId,
        merchantOperatingCityId = Just merchantOpCityId,
        serviceConfig,
        runInCloud = Nothing,
        createdAt = now,
        updatedAt = now
      }
    merchantOpCityId
  clearServiceConfigCaches
  Admin.refreshWhenStored now readStoredUpdatedAt clearServiceConfigCaches
  where
    svcName = getServiceName serviceConfig
    clearServiceConfigCaches = do
      CQMSC.clearCache svcName merchantOpCityId
      CQMSC.clearCacheById merchantOpCityId
      invalidateConfigInMem LYT.MerchantServiceConfigDriver
    readStoredUpdatedAt = do
      rows <-
        getConfigFromDB
          CPMSC.MerchantServiceConfigDimensions
            { CPMSC.merchantOperatingCityId = merchantOpCityId.getId,
              CPMSC.merchantId = Nothing,
              CPMSC.serviceName = Just svcName
            }
      pure $ listToMaybe [row.updatedAt | row <- rows, getServiceName row.serviceConfig == svcName]

findMerchantAndCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, Id DMOC.MerchantOperatingCity)
findMerchantAndCity merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  pure (merchant, merchantOpCityId)
