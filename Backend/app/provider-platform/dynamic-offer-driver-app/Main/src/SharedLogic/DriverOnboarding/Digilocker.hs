module SharedLogic.DriverOnboarding.Digilocker
  ( getDigiLockerConfig,
    verifyDigiLockerEnabled,
    getAllowedVehicleCategories,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import qualified Kernel.External.Verification.Digilocker.Types as DigilockerTypes
import qualified Kernel.External.Verification.Interface as Verification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as CQTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error

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

-- | Get allowed vehicle categories for DigiLocker from config
-- Note: Assumes DigiLocker is already enabled (checked before calling this function)
-- Uses getDigiLockerConfig to get the typed config, then extracts allowedVehicleCategories
-- The field is mandatory in the config type, with a default provided during JSON parsing
getAllowedVehicleCategories :: Id DMOC.MerchantOperatingCity -> Flow [DVC.VehicleCategory]
getAllowedVehicleCategories merchantOpCityId = do
  -- Get the typed DigiLocker config (reuses cached query)
  config <- getDigiLockerConfig merchantOpCityId

  -- Extract allowedVehicleCategories from config and convert Text to VehicleCategory
  let categories = catMaybes $ map textToVehicleCategory config.allowedVehicleCategories

  -- Return parsed categories, or default if parsing failed or empty
  if null categories
    then do
      logInfo $ "DigiLocker getAllowedVehicleCategories - Using default categories for merchantOpCityId: " <> merchantOpCityId.getId <> ". Config had: " <> show config.allowedVehicleCategories
      return defaultAllowedVehicleCategories
    else return categories
  where
    -- Default allowed vehicle categories if parsing fails or config is empty
    defaultAllowedVehicleCategories = [DVC.AUTO_CATEGORY, DVC.CAR, DVC.MOTORCYCLE]

    -- Convert Text to VehicleCategory
    textToVehicleCategory :: Text -> Maybe DVC.VehicleCategory
    textToVehicleCategory txt
      | txt == "AUTO_CATEGORY" = Just DVC.AUTO_CATEGORY
      | txt == "CAR" = Just DVC.CAR
      | txt == "MOTORCYCLE" = Just DVC.MOTORCYCLE
      | txt == "AMBULANCE" = Just DVC.AMBULANCE
      | txt == "TRUCK" = Just DVC.TRUCK
      | txt == "BOAT" = Just DVC.BOAT
      | txt == "BUS" = Just DVC.BUS
      | otherwise = Nothing
