module Domain.Action.Dashboard.Fleet.Onboarding
  ( getOnboardingDocumentConfigs,
    getOnboardingRegisterStatus,
    getOnboardingRegisterVehicleStatus,
    getOnboardingVehicleDocuments,
    castStatusRes,
    postOnboardingVerify,
    getOnboardingGetReferralDetails,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra as OnboardingExtra
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
import qualified Domain.Action.UI.DriverOnboarding.Status as UIStatus
import qualified Domain.Action.UI.DriverOnboarding.UdyamVerification as UDYAM
import qualified Domain.Action.UI.DriverOnboardingV2 as DOnboarding
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.VehicleCategory as DVC
import qualified Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt, encrypt, hash)
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
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery

getOnboardingDocumentConfigs ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe CommonOnboarding.Role ->
  Maybe OnboardingExtra.DocumentOnboardingStage ->
  Environment.Flow CommonOnboarding.DocumentVerificationConfigList
getOnboardingDocumentConfigs merchantShortId opCity fleetOwnerId makeSelfieAadhaarPanMandatory mbOnlyVehicle role documentOnboardingStage = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mbPerson <- runInReplica $ PersonQuery.findById (Id fleetOwnerId)
  let personLanguage = maybe ENGLISH (fromMaybe ENGLISH . language) mbPerson

  fleetConfigsRaw <- case role of
    Just CommonOnboarding.NORMAL_FLEET -> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Just FLEET_OWNER}) (Just (FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_OWNER (Just [])))
    Just CommonOnboarding.BUSINESS_FLEET -> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Just FLEET_BUSINESS}) (Just (FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_BUSINESS (Just [])))
    _ -> pure []

  fleetConfigs <- filterByStage documentOnboardingStage . SDO.filterInCompatibleFlows makeSelfieAadhaarPanMandatory <$> mapM (SDO.mkFleetOwnerDocumentVerificationConfigAPIEntity personLanguage) fleetConfigsRaw

  Onboarding.DocumentVerificationConfigList {..} <- DOnboarding.getOnboardingConfigs' personLanguage merchantOpCityId makeSelfieAadhaarPanMandatory mbOnlyVehicle
  let castConfigs = fmap castDocumentVerificationConfigAPIEntity
  return $
    CommonOnboarding.DocumentVerificationConfigList
      { fleet = SDO.toMaybe fleetConfigs,
        ambulances = fmap castConfigs ambulances,
        autos = fmap castConfigs autos,
        bikes = fmap castConfigs bikes,
        bus = fmap castConfigs bus,
        cabs = fmap castConfigs cabs,
        trucks = fmap castConfigs trucks,
        boat = fmap castConfigs boat,
        toto = fmap castConfigs toto
      }

filterByStage ::
  Maybe OnboardingExtra.DocumentOnboardingStage ->
  [CommonOnboarding.DocumentVerificationConfigAPIEntity] ->
  [CommonOnboarding.DocumentVerificationConfigAPIEntity]
filterByStage mbStage =
  filter (\doc -> maybe True (\stage -> doc.documentOnboardingStage == Just stage) mbStage)

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
      documentOnboardingStage = SDO.castDocumentOnboardingStage <$> documentOnboardingStage,
      isReminderSupported = isReminderSupported,
      isApprovalSupported = isApprovalSupported,
      rolesAllowedToUploadDocument = fmap (mapMaybe SDO.castPersonRoleToDashboardAccessType) rolesAllowedToUploadDocument
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
  Maybe Bool ->
  Environment.Flow CommonOnboarding.StatusRes
getOnboardingRegisterStatus merchantShortId opCity fleetOwnerId mbPersonId makeSelfieAadhaarPanMandatory onboardingVehicleCategory prefillData onlyMandatoryDocs mbDocsVerificationStatusFilter enableDocumentMetadata = do
  let personId = fromMaybe fleetOwnerId ((.getId) <$> mbPersonId)
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  mDL <- DLQuery.findByDriverId (Id personId)
  person <- runInReplica $ PersonQuery.findById (Id personId) >>= fromMaybeM (PersonNotFound personId)
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity = merchantOpCity, entityImages, transporterConfig, now, enableDocumentMetadata = fromMaybe False enableDocumentMetadata}
  let shouldActivateRc = True
      skipMessages = False -- Need translations for API response
  statusRes <- SStatus.statusHandler' person entityImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL (Just True) shouldActivateRc onlyMandatoryDocs skipMessages
  -- Re-pull stuck doc verifications; fleet-owner GST/UDYAM are reachable only via this endpoint.
  UIStatus.pullPendingDocStatuses transporterConfig person statusRes.driverDocuments statusRes.vehicleDocuments
  let res = castStatusRes statusRes
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

-- | Dashboard vehicle-only RC verify-status. Delegates to the shared @rcVerifyStatus@ with @isDashboard =
--   True@ (and no caller), which skips the per-person RC-ownership gate — dashboard callers are already
--   merchant/city-scoped by ApiAuthV2. The RC is resolved from @registrationNo@/@rcId@, but its resolved
--   city must match this dashboard user's operating city (else the RC belongs to a different city).
getOnboardingRegisterVehicleStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Environment.Flow CommonOnboarding.RcVerifyStatusResp
getOnboardingRegisterVehicleStatus merchantShortId opCity mbRegistrationNo mbRcId enableDocumentMetadata = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  (registrationNo, verified, approved, documents) <- DOnboarding.rcVerifyStatus (DOnboarding.DashboardCaller merchantOpCity.id) mbRegistrationNo mbRcId (fromMaybe False enableDocumentMetadata)
  pure $
    CommonOnboarding.RcVerifyStatusResp
      { registrationNo,
        verified,
        approved,
        documents = map castDocumentStatusItem documents
      }

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
          proofDocumentType = DDriver.castToCommon <$> l.proofDocumentType,
          address = l.address
        }
  VehicleDocs.GSTMetadata g ->
    CommonOnboarding.GSTMetadata
      CommonOnboarding.GSTDocumentMetadata
        { gstNumber = g.gstNumber
        }
  VehicleDocs.RCMetadata r ->
    CommonOnboarding.RCMetadata
      CommonOnboarding.RCDocumentMetadata
        { fitnessExpiry = r.fitnessExpiry,
          vehicleNumberPlate = r.vehicleNumberPlate,
          vehicleVariant = r.vehicleVariant,
          vehicleManufacturer = r.vehicleManufacturer,
          vehicleModel = r.vehicleModel,
          vehicleModelYear = r.vehicleModelYear,
          vehicleColor = r.vehicleColor
        }
  VehicleDocs.VehiclePUCMetadata p ->
    CommonOnboarding.VehiclePUCMetadata
      CommonOnboarding.VehiclePUCDocumentMetadata
        { pucNumber = p.pucNumber,
          pucExpiry = p.pucExpiry
        }
  VehicleDocs.VehicleFitnessMetadata f ->
    CommonOnboarding.VehicleFitnessMetadata
      CommonOnboarding.VehicleFitnessCertificateDocumentMetadata
        { fitnessExpiry = f.fitnessExpiry,
          applicationNumber = f.applicationNumber,
          rcNumber = f.rcNumber
        }
  VehicleDocs.VehicleInsuranceMetadata i ->
    CommonOnboarding.VehicleInsuranceMetadata
      CommonOnboarding.VehicleInsuranceDocumentMetadata
        { policyNumber = i.policyNumber,
          insuranceExpiry = i.insuranceExpiry,
          insuranceProvider = i.insuranceProvider,
          rcNumber = i.rcNumber
        }
  VehicleDocs.VehiclePermitMetadata p ->
    CommonOnboarding.VehiclePermitMetadata
      CommonOnboarding.VehiclePermitDocumentMetadata
        { permitNumber = p.permitNumber,
          permitExpiry = p.permitExpiry,
          regionCovered = p.regionCovered,
          rcNumber = p.rcNumber
        }
  VehicleDocs.UDYAMMetadata u ->
    CommonOnboarding.UDYAMMetadata
      CommonOnboarding.UDYAMDocumentMetadata
        { udyamNumber = u.udyamNumber,
          tdsRate = u.tdsRate
        }
  VehicleDocs.TANMetadata t ->
    CommonOnboarding.TANMetadata
      CommonOnboarding.TANDocumentMetadata
        { documentId = t.documentId,
          tdsRate = t.tdsRate
        }
  VehicleDocs.LDCMetadata l ->
    CommonOnboarding.LDCMetadata
      CommonOnboarding.LDCDocumentMetadata
        { documentId = l.documentId,
          tdsRate = l.tdsRate
        }
  VehicleDocs.NomineeDetailsMetadata n ->
    CommonOnboarding.NomineeDetailsMetadata
      CommonOnboarding.NomineeDetailsDocumentMetadata
        { nomineeName = n.nomineeName,
          nomineeDob = n.nomineeDob,
          nomineeRelationship = n.nomineeRelationship
        }
  VehicleDocs.BankingDetailsMetadata b ->
    CommonOnboarding.BankingDetailsMetadata
      CommonOnboarding.BankingDetailsDocumentMetadata
        { accountNumber = b.accountNumber,
          ifscCode = b.ifscCode,
          nameAtBank = b.nameAtBank,
          upiId = b.upiId
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

getOnboardingVehicleDocuments ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Environment.Flow CommonOnboarding.VehicleDocumentStatusRes
getOnboardingVehicleDocuments merchantShortId opCity mbRcNo mbRcId enableDocumentMetadata = do
  when (isNothing mbRcNo && isNothing mbRcId) $ throwError (InvalidRequest "Either rcNo or rcId must be provided")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCity.id Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  rc <- case mbRcId of
    Just rcId -> RCQuery.findById (Id rcId) >>= fromMaybeM (InvalidRequest $ "RC not found by id: " <> rcId)
    Nothing -> case mbRcNo of
      Just rcNo -> do
        rcNoEnc <- encrypt rcNo
        RCQuery.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InvalidRequest $ "RC not found for number: " <> rcNo)
      Nothing -> throwError (InvalidRequest "Either rcNo or rcId must be provided")
  -- Dashboard callers are merchant/city-scoped by ApiAuthV2; the resolved RC must belong to the same operating city.
  whenJust rc.merchantOperatingCityId $ \rcOpCityId ->
    when (rcOpCityId /= merchantOpCity.id) $
      throwError (InvalidRequest "RC does not belong to the requested operating city")
  let entity = IQuery.VehicleRCEntity rc
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity = merchantOpCity, entityImages, transporterConfig, now, enableDocumentMetadata = fromMaybe False enableDocumentMetadata}
  allDocumentVerificationConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOpCity.id Nothing))
  registrationNo <- decrypt rc.certificateNumber
  let skipMessages = True
  vehicleDocuments <- VehicleDocs.fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs ENGLISH (Just registrationNo) Nothing skipMessages
  let mbVehicleDoc = find (\doc -> doc.registrationNo == registrationNo) vehicleDocuments
  return CommonOnboarding.VehicleDocumentStatusRes {vehicleDocument = castVehicleDocumentItem <$> mbVehicleDoc}

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
