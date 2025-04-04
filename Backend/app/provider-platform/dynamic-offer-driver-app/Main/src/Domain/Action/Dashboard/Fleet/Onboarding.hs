module Domain.Action.Dashboard.Fleet.Onboarding
  ( getOnboardingDocumentConfigs,
    getOnboardingRegisterStatus,
    castStatusRes,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding as CommonOnboarding
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as CommonDriverRegistration
import qualified API.Types.UI.DriverOnboardingV2 as Onboarding
import qualified Dashboard.Common
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
import qualified SharedLogic.DriverOnboarding.Status as SStatus
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
getOnboardingRegisterStatus merchantShortId opCity fleetOwnerId mbPersonId makeSelfieAadhaarPanMandatory onboardingVehicleCategory prefillData = do
  let personId = fromMaybe fleetOwnerId ((.getId) <$> mbPersonId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  mDL <- DLQuery.findByDriverId (Id personId)
  let multipleRC = Nothing
  castStatusRes <$> SStatus.statusHandler' (Id personId) merchantOpCity transporterConfig makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL (Just True)

castStatusRes :: SStatus.StatusRes' -> CommonOnboarding.StatusRes
castStatusRes SStatus.StatusRes' {..} =
  CommonOnboarding.StatusRes
    { driverDocuments = castDocumentStatusItem <$> driverDocuments,
      driverLicenseDetails = fmap (castDLDetails <$>) driverLicenseDetails,
      vehicleDocuments = castVehicleDocumentItem <$> vehicleDocuments,
      vehicleRegistrationCertificateDetails = fmap (castRCDetails <$>) vehicleRegistrationCertificateDetails,
      ..
    }

castDocumentStatusItem :: SStatus.DocumentStatusItem -> CommonOnboarding.DocumentStatusItem
castDocumentStatusItem SStatus.DocumentStatusItem {..} =
  CommonOnboarding.DocumentStatusItem
    { documentType = SDO.castDocumentType documentType,
      verificationStatus = castResponseStatus verificationStatus,
      ..
    }

castDLDetails :: SStatus.DLDetails -> CommonDriverRegistration.DLDetails
castDLDetails SStatus.DLDetails {..} = CommonDriverRegistration.DLDetails {..}

castRCDetails :: SStatus.RCDetails -> CommonDriverRegistration.RCDetails
castRCDetails SStatus.RCDetails {..} =
  CommonDriverRegistration.RCDetails
    { ..
    }

castVehicleDocumentItem :: SStatus.VehicleDocumentItem -> CommonOnboarding.VehicleDocumentItem
castVehicleDocumentItem SStatus.VehicleDocumentItem {..} =
  CommonOnboarding.VehicleDocumentItem
    { documents = castDocumentStatusItem <$> documents,
      ..
    }

castResponseStatus :: SStatus.ResponseStatus -> CommonOnboarding.ResponseStatus
castResponseStatus = \case
  SStatus.NO_DOC_AVAILABLE -> CommonOnboarding.NO_DOC_AVAILABLE
  SStatus.PENDING -> CommonOnboarding.PENDING
  SStatus.VALID -> CommonOnboarding.VALID
  SStatus.FAILED -> CommonOnboarding.FAILED
  SStatus.INVALID -> CommonOnboarding.INVALID
  SStatus.LIMIT_EXCEED -> CommonOnboarding.LIMIT_EXCEED
  SStatus.MANUAL_VERIFICATION_REQUIRED -> CommonOnboarding.MANUAL_VERIFICATION_REQUIRED
  SStatus.UNAUTHORIZED -> CommonOnboarding.UNAUTHORIZED
