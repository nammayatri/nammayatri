module Domain.Action.Dashboard.Fleet.Onboarding
  ( getOnboardingDocumentConfigs,
    getOnboardingRegisterStatus,
    castStatusRes,
    postOnboardingVerify,
    getOnboardingGetReferralDetails,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding as CommonOnboarding
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as CommonDriverRegistration
import qualified API.Types.UI.DriverOnboardingV2 as Onboarding
import qualified Dashboard.Common
import qualified Data.Text as Text
import Domain.Action.UI.DriverOnboarding.Referral
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DVRC
import qualified Domain.Action.UI.DriverOnboardingV2 as DOnboarding
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.VehicleCategory as DVC
import qualified Environment
import Kernel.Beam.Functions
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.Image as IQuery
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

getOnboardingGetReferralDetails ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Environment.Flow CommonOnboarding.ReferralInfoRes
getOnboardingGetReferralDetails merchantShortId opCity requestorId referralCode = do
  when (Text.length referralCode < 6) $ throwError (InvalidRequest "Referral code should be at least 6 digits long")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  dr <- validateReferralCodeAndRole transporterConfig (Id requestorId) referralCode (Just OPERATOR)
  person <- PersonQuery.findById dr.driverId >>= fromMaybeM (PersonNotFound dr.driverId.getId)
  return $
    CommonOnboarding.ReferralInfoRes
      { personId = cast dr.driverId,
        name = person.firstName <> " " <> (fromMaybe "" person.middleName) <> " " <> (fromMaybe "" person.lastName)
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
  driverImages <- IQuery.findAllByPersonId transporterConfig (Id personId)
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = Id personId, merchantOperatingCity = merchantOpCity, driverImages, transporterConfig, now}
  castStatusRes <$> SStatus.statusHandler' driverImagesInfo makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL (Just True)

postOnboardingVerify ::
  ShortId DM.Merchant ->
  Context.City ->
  CommonOnboarding.VerifyType ->
  CommonOnboarding.VerifyReq ->
  Environment.Flow APISuccess
postOnboardingVerify merchantShortId opCity reqType req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  _transporterConfig <- findByMerchantOpCityId merchantOpCity.id Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  case reqType of
    CommonOnboarding.VERIFY_PAN -> DVRC.verifyPan True (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DVRC.DriverPanReq {panNumber = req.identifierNumber, imageId = req.imageId, driverId = req.driverId})
    CommonOnboarding.VERIFY_GST -> DVRC.verifyGstin True (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DVRC.DriverGstinReq {gstin = req.identifierNumber, imageId = req.imageId, driverId = req.driverId})
    CommonOnboarding.VERIFY_AADHAAR -> DVRC.verifyAadhaar True (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DVRC.DriverAadhaarReq {aadhaarNumber = req.identifierNumber, aadhaarFrontImageId = req.imageId, aadhaarBackImageId = req.optionalImageId, consent = True, driverId = req.driverId})

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
