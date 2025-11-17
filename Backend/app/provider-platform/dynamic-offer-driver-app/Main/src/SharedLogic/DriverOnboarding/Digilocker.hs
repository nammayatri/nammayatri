module SharedLogic.DriverOnboarding.Digilocker
  ( DocStatus (..),
    docStatusToText,
    getDigiLockerConfig,
    verifyDigiLockerEnabled,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import qualified Kernel.External.Verification.Digilocker.Types as DigilockerTypes
import qualified Kernel.External.Verification.Interface as Verification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

data DocStatus
  = DOC_PENDING
  | DOC_SUCCESS
  | DOC_FAILED
  | DOC_CONSENT_DENIED
  | DOC_PULL_REQUIRED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

docStatusToText :: DocStatus -> Text
docStatusToText status = case status of
  DOC_PENDING -> "PENDING"
  DOC_SUCCESS -> "SUCCESS"
  DOC_FAILED -> "FAILED"
  DOC_CONSENT_DENIED -> "CONSENT_DENIED"
  DOC_PULL_REQUIRED -> "PULL_REQUIRED"

getDigiLockerConfig :: Id DMOC.MerchantOperatingCity -> Flow DigilockerTypes.DigiLockerCfg
getDigiLockerConfig merchantOpCityId = do
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  unless (transporterConfig.digilockerEnabled == Just True) $
    throwError $ InvalidRequest "DigiLocker not enabled for this merchant"

  let serviceName = DMSC.VerificationService Verification.DigiLocker
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOpCityId
      >>= fromMaybeM (InternalError "DigiLocker service config not found. Please configure DigiLocker in merchant_service_config table.")

  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig (Verification.DigiLockerConfig config) ->
      return config
    _ -> throwError $ InternalError "Invalid DigiLocker service config type"

verifyDigiLockerEnabled :: Id DMOC.MerchantOperatingCity -> Flow ()
verifyDigiLockerEnabled merchantOpCityId = do
  transporterConfig <-
    CQTC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)

  unless (fromMaybe False transporterConfig.digilockerEnabled) $
    throwError $ InvalidRequest "DigiLocker is not enabled for this merchant+city"

  logInfo $ "DigiLocker initiate - Verified DigiLocker is enabled for merchantOpCityId: " <> merchantOpCityId.getId
