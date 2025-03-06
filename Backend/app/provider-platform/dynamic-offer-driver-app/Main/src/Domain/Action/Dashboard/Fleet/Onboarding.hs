module Domain.Action.Dashboard.Fleet.Onboarding (getOnboardingDocumentConfigs, getOnboardingRegisterStatus) where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding as CommonOnboarding
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as CommonDriverRegistration
import qualified API.Types.UI.DriverOnboardingV2 as Onboarding
import qualified Dashboard.Common
import qualified Domain.Action.UI.DriverOnboarding.Status as DStatus
import qualified Domain.Action.UI.DriverOnboardingV2 as DOnboarding
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.VehicleCategory as DVC
import qualified Environment
import Kernel.Beam.Functions
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.Person as PersonQuery

getOnboardingDocumentConfigs ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe CommonOnboarding.Role ->
  Environment.Flow CommonOnboarding.DocumentVerificationConfigList
getOnboardingDocumentConfigs merchantShortId opCity fleetOwnerId makeSelfieAadhaarPanMandatory mbOnlyVehicle role = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mbPerson <- runInReplica $ PersonQuery.findById (Id fleetOwnerId)
  let personLanguage = maybe ENGLISH (fromMaybe ENGLISH . language) mbPerson

  fleetConfigsRaw <- case role of
    Just CommonOnboarding.NORMAL_FLEET -> FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_OWNER
    Just CommonOnboarding.BUSINESS_FLEET -> FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_BUSINESS
    _ -> pure []

  fleetConfigs <- SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (SDO.mkFleetOwnerDocumentVerificationConfigAPIEntity personLanguage) fleetConfigsRaw

  Onboarding.DocumentVerificationConfigList {..} <- DOnboarding.getOnboardingConfigs' personLanguage merchantOpCityId makeSelfieAadhaarPanMandatory mbOnlyVehicle
  return $
    CommonOnboarding.DocumentVerificationConfigList
      { fleet = SDO.toMaybe fleetConfigs,
        ambulances = fmap (castDocumentVerificationConfigAPIEntity <$>) ambulances,
        autos = fmap (castDocumentVerificationConfigAPIEntity <$>) autos,
        bikes = fmap (castDocumentVerificationConfigAPIEntity <$>) bikes,
        bus = fmap (castDocumentVerificationConfigAPIEntity <$>) bus,
        cabs = fmap (castDocumentVerificationConfigAPIEntity <$>) cabs,
        trucks = fmap (castDocumentVerificationConfigAPIEntity <$>) trucks
      }

castDocumentVerificationConfigAPIEntity :: Onboarding.DocumentVerificationConfigAPIEntity -> CommonOnboarding.DocumentVerificationConfigAPIEntity
castDocumentVerificationConfigAPIEntity Onboarding.DocumentVerificationConfigAPIEntity {..} =
  CommonOnboarding.DocumentVerificationConfigAPIEntity
    { dependencyDocumentType = SDO.castDocumentType <$> dependencyDocumentType,
      documentType = SDO.castDocumentType documentType,
      documentCategory = SDO.castDocumentCategory <$> documentCategory,
      ..
    }

getOnboardingRegisterStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe (Id Dashboard.Common.Driver) ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe Bool ->
  Environment.Flow CommonOnboarding.StatusRes
getOnboardingRegisterStatus merchantShortId opCity personId _ makeSelfieAadhaarPanMandatory onboardingVehicleCategory prefillData = do
  -- fleetOwnerId
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId) -- (Just (DriverId (cast personId)))
  mDL <- DLQuery.findByDriverId (Id personId)
  let multipleRC = Nothing
  castStatusRes <$> DStatus.statusHandler' (Id personId) merchantOpCity transporterConfig makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL

castStatusRes :: DStatus.StatusRes' -> CommonOnboarding.StatusRes
castStatusRes DStatus.StatusRes' {..} =
  CommonOnboarding.StatusRes
    { driverDocuments = castDocumentStatusItem <$> driverDocuments,
      driverLicenseDetails = fmap (castDLDetails <$>) driverLicenseDetails,
      vehicleDocuments = castVehicleDocumentItem <$> vehicleDocuments,
      vehicleRegistrationCertificateDetails = fmap (castRCDetails <$>) vehicleRegistrationCertificateDetails,
      ..
    }

castDocumentStatusItem :: DStatus.DocumentStatusItem -> CommonOnboarding.DocumentStatusItem
castDocumentStatusItem DStatus.DocumentStatusItem {..} =
  CommonOnboarding.DocumentStatusItem
    { documentType = SDO.castDocumentType documentType,
      verificationStatus = castResponseStatus verificationStatus,
      ..
    }

castDLDetails :: DStatus.DLDetails -> CommonDriverRegistration.DLDetails
castDLDetails DStatus.DLDetails {..} = CommonDriverRegistration.DLDetails {..}

castRCDetails :: DStatus.RCDetails -> CommonDriverRegistration.RCDetails
castRCDetails DStatus.RCDetails {..} =
  CommonDriverRegistration.RCDetails
    { ..
    }

castVehicleDocumentItem :: DStatus.VehicleDocumentItem -> CommonOnboarding.VehicleDocumentItem
castVehicleDocumentItem DStatus.VehicleDocumentItem {..} =
  CommonOnboarding.VehicleDocumentItem
    { documents = castDocumentStatusItem <$> documents,
      ..
    }

castResponseStatus :: DStatus.ResponseStatus -> CommonOnboarding.ResponseStatus
castResponseStatus = \case
  DStatus.NO_DOC_AVAILABLE -> CommonOnboarding.NO_DOC_AVAILABLE
  DStatus.PENDING -> CommonOnboarding.PENDING
  DStatus.VALID -> CommonOnboarding.VALID
  DStatus.FAILED -> CommonOnboarding.FAILED
  DStatus.INVALID -> CommonOnboarding.INVALID
  DStatus.LIMIT_EXCEED -> CommonOnboarding.LIMIT_EXCEED
  DStatus.MANUAL_VERIFICATION_REQUIRED -> CommonOnboarding.MANUAL_VERIFICATION_REQUIRED
  DStatus.UNAUTHORIZED -> CommonOnboarding.UNAUTHORIZED
