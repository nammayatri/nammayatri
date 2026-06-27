module Domain.Action.Dashboard.Fleet.Onboarding
  ( getOnboardingDocumentConfigs,
    getOnboardingRegisterStatus,
    castStatusRes,
    postOnboardingVerify,
    getOnboardingGetReferralDetails,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding as CommonOnboarding
import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified API.Types.ProviderPlatform.Management.DriverRegistration as CommonDriverRegistration
import qualified API.Types.UI.DriverOnboardingV2 as Onboarding
import qualified Dashboard.Common
import qualified Data.Text as Text
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Management.Driver as DDriver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as DAV
import qualified Domain.Action.UI.DriverOnboarding.GstVerification as DGV
import qualified Domain.Action.UI.DriverOnboarding.PanVerification as DPV
import Domain.Action.UI.DriverOnboarding.Referral
import qualified Domain.Action.UI.DriverOnboarding.UdyamVerification as UDYAM
import qualified Domain.Action.UI.DriverOnboardingV2 as DOnboarding
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverPanCard as DPan
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
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified SharedLogic.DriverOnboarding.VehicleDocs as VehicleDocs
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
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
    Just CommonOnboarding.NORMAL_FLEET -> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Just FLEET_OWNER}) (Just (FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_OWNER (Just [])))
    Just CommonOnboarding.BUSINESS_FLEET -> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Just FLEET_BUSINESS}) (Just (FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_BUSINESS (Just [])))
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
        trucks = fmap (castDocumentVerificationConfigAPIEntity <$>) trucks,
        boat = fmap (castDocumentVerificationConfigAPIEntity <$>) boat,
        toto = fmap (castDocumentVerificationConfigAPIEntity <$>) toto
      }

castDocumentVerificationConfigAPIEntity :: Onboarding.DocumentVerificationConfigAPIEntity -> CommonOnboarding.DocumentVerificationConfigAPIEntity
castDocumentVerificationConfigAPIEntity Onboarding.DocumentVerificationConfigAPIEntity {..} =
  CommonOnboarding.DocumentVerificationConfigAPIEntity
    { title = title,
      description = description,
      checkExpiry = checkExpiry,
      checkExtraction = checkExtraction,
      dependencyDocumentType = SDO.castDocumentType <$> dependencyDocumentType,
      disableWarning = disableWarning,
      documentCategory = SDO.castDocumentCategory <$> documentCategory,
      documentType = SDO.castDocumentType documentType,
      filterForOldApks = filterForOldApks,
      isDisabled = isDisabled,
      isHidden = isHidden,
      isMandatory = isMandatory,
      isMandatoryForEnabling = isMandatoryForEnabling,
      rcNumberPrefixList = rcNumberPrefixList,
      applicableTo = SDO.castDocumentApplicableType applicableTo,
      documentFields = fmap (map SDO.castDocumentFieldInfo) documentFields,
      documentFlowGrouping = SDO.castDocumentFlowGrouping documentFlowGrouping,
      isReminderSupported = isReminderSupported,
      isApprovalSupported = isApprovalSupported
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
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
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
  Maybe Bool ->
  Maybe Dashboard.Common.DocsVerificationStatus ->
  Environment.Flow CommonOnboarding.StatusRes
getOnboardingRegisterStatus merchantShortId opCity fleetOwnerId mbPersonId makeSelfieAadhaarPanMandatory onboardingVehicleCategory prefillData onlyMandatoryDocs mbDocsVerificationStatusFilter = do
  let personId = fromMaybe fleetOwnerId ((.getId) <$> mbPersonId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  mDL <- DLQuery.findByDriverId (Id personId)
  person <- runInReplica $ PersonQuery.findById (Id personId) >>= fromMaybeM (PersonNotFound personId)
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity = merchantOpCity, entityImages, transporterConfig, now}
  let shouldActivateRc = True
      skipMessages = False -- Need translations for API response
  res <- castStatusRes <$> SStatus.statusHandler' person entityImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL (Just True) shouldActivateRc onlyMandatoryDocs skipMessages
  -- When no filter is provided and enableManualDocumentStatusCheck is enabled,
  -- default to ADMIN_APPROVED so only fully-approved vehicles are shown.
  let effectiveFilter =
        case mbDocsVerificationStatusFilter of
          Just _ -> mbDocsVerificationStatusFilter
          Nothing ->
            if transporterConfig.enableManualDocumentStatusCheck == Just True
              then Just Dashboard.Common.ADMIN_APPROVED
              else Nothing
  pure $ applyVehicleDocsFilter effectiveFilter res

postOnboardingVerify ::
  ShortId DM.Merchant ->
  Context.City ->
  CommonOnboarding.VerifyType ->
  Maybe Common.DashboardAccessType ->
  Maybe Bool ->
  CommonOnboarding.VerifyReq ->
  Environment.Flow CommonOnboarding.VerifyDocumentRes
postOnboardingVerify merchantShortId opCity reqType mbAccessType adminApprovalRequired req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let verifyBy = case mbAccessType of
        Just accessTypeValue -> case accessTypeValue of
          Common.DASHBOARD_ADMIN -> DPan.DASHBOARD_ADMIN
          Common.DASHBOARD_USER -> DPan.DASHBOARD_USER
          _ -> DPan.DASHBOARD
        Nothing -> DPan.DASHBOARD
  enable <- case reqType of
    CommonOnboarding.VERIFY_PAN -> DPV.verifyPan verifyBy (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DPV.DriverPanReq {panNumber = req.identifierNumber, imageId = req.imageId, driverId = req.driverId}) adminApprovalRequired req.identifierName True
    CommonOnboarding.VERIFY_GST -> DGV.verifyGstin verifyBy (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DGV.DriverGstinReq {gstin = req.identifierNumber, imageId = req.imageId, driverId = req.driverId}) adminApprovalRequired True
    CommonOnboarding.VERIFY_AADHAAR -> DAV.verifyAadhaar verifyBy (Just merchant) (Id req.driverId, merchant.id, merchantOpCity.id) (DAV.DriverAadhaarReq {aadhaarNumber = Just req.identifierNumber, aadhaarFrontImageId = req.imageId, aadhaarBackImageId = req.optionalImageId, consent = True, driverId = req.driverId, aadhaarName = Nothing}) adminApprovalRequired
    CommonOnboarding.VERIFY_UDYAM -> UDYAM.verifyUdyam (Id req.driverId, merchantOpCity.id) (UDYAM.DriverUdyamReq {uamNumber = req.identifierNumber, imageId1 = Id req.imageId})
  return
    CommonOnboarding.VerifyDocumentRes
      { enableFleetOwner = enable
      }

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
      expiryDate = documentExpiry,
      metadata = castDocumentMetadata <$> metadata,
      ..
    }

castDocumentMetadata :: VehicleDocs.DocumentMetadata -> CommonOnboarding.DocumentMetadata
castDocumentMetadata = \case
  VehicleDocs.DLMetadata dl ->
    CommonOnboarding.DLMetadata
      CommonOnboarding.DLDocumentMetadata
        { driverLicenseNumber = dl.driverLicenseNumber,
          driverDateOfBirth = dl.driverDateOfBirth,
          dateOfExpiry = dl.dateOfExpiry
        }
  VehicleDocs.AadhaarMetadata a ->
    CommonOnboarding.AadhaarMetadata
      CommonOnboarding.AadhaarDocumentMetadata
        { aadhaarNumber = a.aadhaarNumber,
          nameOnCard = a.nameOnCard,
          dateOfBirth = a.dateOfBirth,
          address = a.address
        }
  VehicleDocs.PanMetadata p ->
    CommonOnboarding.PanMetadata
      CommonOnboarding.PanDocumentMetadata
        { panNumber = p.panNumber,
          panDocType = castPanType <$> p.panDocType,
          driverDob = p.driverDob
        }
  VehicleDocs.LocalAddressProofMetadata l ->
    CommonOnboarding.LocalAddressProofMetadata
      CommonOnboarding.LocalAddressProofDocumentMetadata
        { state = l.state,
          proofDocumentType = DDriver.castToCommon <$> l.proofDocumentType
        }

castPanType :: DPan.PanType -> CommonDriverRegistration.PanType
castPanType DPan.INDIVIDUAL = CommonDriverRegistration.INDIVIDUAL
castPanType DPan.BUSINESS = CommonDriverRegistration.BUSINESS

castDLDetails :: SStatus.DLDetails -> CommonDriverRegistration.DLDetails
castDLDetails SStatus.DLDetails {..} = CommonDriverRegistration.DLDetails {..}

castRCDetails :: SStatus.RCDetails -> CommonDriverRegistration.RCDetails
castRCDetails SStatus.RCDetails {..} =
  CommonDriverRegistration.RCDetails
    { verificationStatus = DCommon.castVerificationStatus <$> verificationStatus,
      ..
    }

castVehicleDocumentItem :: SStatus.VehicleDocumentItem -> CommonOnboarding.VehicleDocumentItem
castVehicleDocumentItem SStatus.VehicleDocumentItem {..} =
  CommonOnboarding.VehicleDocumentItem
    { documents = castDocumentStatusItem <$> documents,
      expiryDate = documentExpiry,
      docsVerificationStatus = castDocsVerificationStatus <$> docsVerificationStatus,
      ..
    }

castDocsVerificationStatus :: DDVS.DocsVerificationStatus -> Dashboard.Common.DocsVerificationStatus
castDocsVerificationStatus = \case
  DDVS.ADMIN_PENDING -> Dashboard.Common.ADMIN_PENDING
  DDVS.ADMIN_APPROVED -> Dashboard.Common.ADMIN_APPROVED
  DDVS.ADMIN_REJECTED -> Dashboard.Common.ADMIN_REJECTED

applyVehicleDocsFilter :: Maybe Dashboard.Common.DocsVerificationStatus -> CommonOnboarding.StatusRes -> CommonOnboarding.StatusRes
applyVehicleDocsFilter Nothing res = res
applyVehicleDocsFilter (Just target) res =
  res {CommonOnboarding.vehicleDocuments = filter (\v -> CommonOnboarding.docsVerificationStatus v == Just target) (CommonOnboarding.vehicleDocuments res)}

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
  SStatus.PULL_REQUIRED -> CommonOnboarding.PULL_REQUIRED
  SStatus.CONSENT_DENIED -> CommonOnboarding.CONSENT_DENIED
