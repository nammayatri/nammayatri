module SharedLogic.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes' (..),
    VehicleDocumentItem (..),
    DocumentStatusItem (..),
    CommonDocumentItem (..),
    DLDetails (..),
    RCDetails (..),
    statusHandler',
    getDLAndStatus,
    getRCAndStatus,
    getAadhaarStatus,
    mapStatus,
    checkAllDriverVehicleDocsVerified,
    checkAllVehicleDocsVerifiedForRC,
    checkAllDriverDocsVerifiedForDriver,
    activateRCAutomatically,
    mkCommonDocumentItem,
    checkInspectionHubRequestCreated,
    getInspectionHubStatusForResponseStatus,
  )
where

import Control.Applicative ((<|>))
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Plan as DAPlan
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCDOD
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverLicense as DL
import qualified Domain.Types.HyperVergeVerification as HV
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.OperationHubRequests as DOHR
import qualified Domain.Types.Person as DP
import Domain.Types.Plan as Plan
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Plasma as Plasma
import Kernel.External.Types
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Digilocker as SDDigilocker
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as BVQuery
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverGstin as QDGST
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverSSN as QDSSN
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.OperationHubRequestsExtra as QOHRE
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleFitnessCertificate as VFCQuery
import qualified Storage.Queries.VehicleInsurance as VIQuery
import qualified Storage.Queries.VehicleNOC as VNOCQuery
import qualified Storage.Queries.VehiclePUC as VPUCQuery
import qualified Storage.Queries.VehiclePermit as VPQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.BackgroundVerification as BackgroundVerification
import Tools.Error (DriverOnboardingError (ImageNotValid))
import qualified Tools.SMS as Sms
import qualified Tools.Verification as Verification

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- UNAUTHORIZED is used when a driver is not eligible to be onboarded to the platform
-- INVALID is the state
--   which the doc switches to when, for example, it's expired or when it is invalidated from dashboard.
-- PULL_REQUIRED is used when a document needs to be pulled from DigiLocker
-- CONSENT_DENIED is used when user denies consent for DigiLocker verification
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED | UNAUTHORIZED | PULL_REQUIRED | CONSENT_DENIED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes' = StatusRes'
  { driverDocuments :: [DocumentStatusItem],
    vehicleDocuments :: [VehicleDocumentItem],
    enabled :: Bool,
    manualVerificationRequired :: Maybe Bool,
    driverLicenseDetails :: Maybe [DLDetails],
    vehicleRegistrationCertificateDetails :: Maybe [RCDetails],
    digilockerResponseCode :: Maybe Text,
    digilockerAuthorizationUrl :: Maybe Text
  }

data VehicleDocumentItem = VehicleDocumentItem
  { registrationNo :: Text,
    userSelectedVehicleCategory :: DVC.VehicleCategory,
    verifiedVehicleCategory :: Maybe DVC.VehicleCategory,
    isVerified :: Bool,
    isActive :: Bool,
    isApproved :: Bool,
    vehicleModel :: Maybe Text,
    documents :: [DocumentStatusItem],
    dateOfUpload :: UTCTime,
    s3Path :: Maybe Text,
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: DDVC.DocumentType,
    verificationStatus :: ResponseStatus,
    verificationMessage :: Maybe Text,
    verificationUrl :: Maybe BaseUrl,
    s3Path :: Maybe Text,
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CommonDocumentItem = CommonDocumentItem
  { documentType :: DDVC.DocumentType,
    documentData :: Text,
    verificationStatus :: ResponseStatus,
    rejectReason :: Maybe Text,
    documentImageId :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DLDetails = DLDetails
  { driverName :: Maybe Text,
    driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: Maybe UTCTime,
    classOfVehicles :: [Text],
    imageId1 :: Text,
    imageId2 :: Maybe (Text),
    dateOfIssue :: Maybe UTCTime,
    createdAt :: UTCTime,
    s3Path1 :: Maybe Text,
    s3Path2 :: Maybe Text,
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Text,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe Text,
    airConditioned :: Maybe Bool,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    vehicleModelYear :: Maybe Int,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    createdAt :: UTCTime,
    failedRules :: [Text],
    verificationStatus :: Maybe Documents.VerificationStatus,
    s3Path :: Maybe Text,
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

checkAllDriverVehicleDocsVerified ::
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Flow Bool
checkAllDriverVehicleDocsVerified person merchantOperatingCity transporterConfig language reqRegistrationNo = do
  let personId = person.id
  let onlyMandatoryDocs = Just True
  let useHVSdkForDL = Just True
  driverImages <- IQuery.findAllByPersonId transporterConfig personId
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = Just personId, merchantOperatingCity, driverImages, transporterConfig, now}
  allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing
  vehicleDocumentsUnverified <-
    if isFleetRole person.role
      then pure []
      else fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs

  let possibleVehicleCategoriesRaw = nub $ do
        vehicleDocumentsUnverified <&> \vehicleDoc -> do
          fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      -- Ensure at least one category is provided to avoid empty list bug
      -- If no vehicle categories found, use CAR as fallback (same as vehicleCategory fallback used later)
      possibleVehicleCategories = if null possibleVehicleCategoriesRaw then [DVC.CAR] else possibleVehicleCategoriesRaw
  driverDocuments <- fetchDriverDocuments driverImagesInfo allDocumentVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs
  vehicleDoc <-
    find (\doc -> doc.registrationNo == reqRegistrationNo) vehicleDocumentsUnverified
      & fromMaybeM (InvalidRequest $ "Vehicle doc not found for driverId " <> personId.getId <> " with registartionNo " <> reqRegistrationNo)
  let makeSelfieAadhaarPanMandatory = Nothing
      vehicleCategory = fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      allVehicleDocsVerified = checkAllVehicleDocsVerified allDocumentVerificationConfigs vehicleDoc makeSelfieAadhaarPanMandatory
      allDriverDocsVerified = checkAllDriverDocsVerified allDocumentVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
  pure $ allVehicleDocsVerified && allDriverDocsVerified

-- Check only vehicle docs for a specific RC (used for vehicle inspection approval)
-- mbPerson = Nothing for vehicle inspection, without driver linked to vehicle
checkAllVehicleDocsVerifiedForRC ::
  Maybe DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Flow Bool
checkAllVehicleDocsVerifiedForRC mbPerson merchantOperatingCity transporterConfig language reqRegistrationNo = do
  let mbPersonId = mbPerson <&> (.id)
  let onlyMandatoryDocs = Just True
  driverImages <- case mbPersonId of
    Just personId -> IQuery.findAllByPersonId transporterConfig personId
    Nothing -> do
      mbRc <- RCQuery.findLastVehicleRCWrapper reqRegistrationNo
      case mbRc of
        Nothing -> pure []
        Just rc -> IQuery.findAllByRcId (Just rc.id.getId) -- TODO refactor later
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = mbPersonId, merchantOperatingCity, driverImages, transporterConfig, now}
  allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing
  let personRole = maybe DP.DRIVER (.role) mbPerson
  vehicleDocumentsUnverified <-
    if isFleetRole personRole
      then pure []
      else fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs
  vehicleDoc <-
    find (\doc -> doc.registrationNo == reqRegistrationNo) vehicleDocumentsUnverified
      & fromMaybeM (InvalidRequest $ "Vehicle doc not found for vehicle with registartionNo " <> reqRegistrationNo)
  let makeSelfieAadhaarPanMandatory = Nothing
  pure $ checkAllVehicleDocsVerified allDocumentVerificationConfigs vehicleDoc makeSelfieAadhaarPanMandatory

-- Check only driver docs (used for driver inspection approval)
checkAllDriverDocsVerifiedForDriver ::
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow Bool
checkAllDriverDocsVerifiedForDriver person merchantOperatingCity transporterConfig language = do
  let personId = person.id
  let onlyMandatoryDocs = Just True
  let useHVSdkForDL = Just True
  driverImages <- IQuery.findAllByPersonId transporterConfig personId
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = Just personId, merchantOperatingCity, driverImages, transporterConfig, now}
  allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing
  -- Get vehicle categories from any associated vehicles to determine required driver docs
  vehicleDocumentsUnverified <-
    if isFleetRole person.role
      then pure []
      else fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language Nothing onlyMandatoryDocs
  let possibleVehicleCategoriesRaw = nub $ do
        vehicleDocumentsUnverified <&> \vehicleDoc -> do
          fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      -- Ensure at least one category is provided to avoid empty list bug
      -- If no vehicle categories found, use CAR as fallback (same as vehicleCategory fallback below)
      possibleVehicleCategories = if null possibleVehicleCategoriesRaw then [DVC.CAR] else possibleVehicleCategoriesRaw
  driverDocuments <- fetchDriverDocuments driverImagesInfo allDocumentVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs
  let makeSelfieAadhaarPanMandatory = Nothing
      -- Use first vehicle doc category if available, otherwise use CAR as default (only needed for determining required driver docs)
      vehicleCategory = case vehicleDocumentsUnverified of
        (doc : _) -> fromMaybe doc.userSelectedVehicleCategory doc.verifiedVehicleCategory
        [] -> DVC.CAR
  pure $ checkAllDriverDocsVerified allDocumentVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory

statusHandler' ::
  DP.Person ->
  IQuery.DriverImagesInfo ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe DL.DriverLicense ->
  Maybe Bool ->
  Bool ->
  Maybe Bool ->
  Flow StatusRes'
statusHandler' person driverImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc onlyMandatoryDocs = do
  let merchantId = driverImagesInfo.merchantOperatingCity.merchantId
      merchantOperatingCity = driverImagesInfo.merchantOperatingCity
      merchantOpCityId = merchantOperatingCity.id
      transporterConfig = driverImagesInfo.transporterConfig
      personId = person.id
  let language = fromMaybe merchantOperatingCity.language person.language

  allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOpCityId Nothing
  let mbReqRegistrationNo = Nothing
  vehicleDocumentsUnverified <-
    if isFleetRole person.role
      then pure []
      else fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language mbReqRegistrationNo onlyMandatoryDocs

  let vehicleCategoryWithoutMandatoryConfigs = case onboardingVehicleCategory <|> (mDL >>= (.vehicleCategory)) of
        Just vehicleCategory -> do
          let vehicleDocumentVerificationConfigs = filter (\config -> config.vehicleCategory == vehicleCategory) allDocumentVerificationConfigs
          let mandatoryVehicleDocumentVerificationConfigs = filter (\config -> config.documentType `elem` SDO.defaultVehicleDocumentTypes && fromMaybe config.isMandatory config.isMandatoryForEnabling) vehicleDocumentVerificationConfigs
          if null mandatoryVehicleDocumentVerificationConfigs then Just vehicleCategory else Nothing
        Nothing -> Nothing

  let possibleVehicleCategoriesRaw = nub $
        (maybeToList vehicleCategoryWithoutMandatoryConfigs <>) $ do
          vehicleDocumentsUnverified <&> \vehicleDoc -> do
            fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      -- Ensure at least one category is provided to avoid empty list bug
      -- If no vehicle categories found, use CAR as fallback (same as fallback used later in vehicleCategory)
      possibleVehicleCategories = if null possibleVehicleCategoriesRaw then [DVC.CAR] else possibleVehicleCategoriesRaw

  driverDocuments <- fetchDriverDocuments driverImagesInfo allDocumentVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs

  -- Conditional logic based on separateDriverVehicleEnablement flag
  vehicleDocuments <-
    if transporterConfig.separateDriverVehicleEnablement == Just True
      then do
        -- Separated enablement: Check driver and vehicle enablement separately
        -- Check driver enablement separately (only driver docs + driver inspection)
        when (person.role == DP.DRIVER) $ do
          let vehicleCategory = fromMaybe DVC.CAR $ onboardingVehicleCategory <|> listToMaybe possibleVehicleCategories
              allDriverDocsVerified = all (\doc -> checkIfDocumentValid allDocumentVerificationConfigs doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments
          when allDriverDocsVerified $ do
            -- Check driver inspection approval separately from vehicle inspection
            driverInfo <- runInReplica $ DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
            let driverInspectionNotRequired = transporterConfig.requiresDriverOnboardingInspection /= Just True || driverInfo.approved == Just True
            when driverInspectionNotRequired $ do
              enableDriver merchantOpCityId personId (mDL >>= (.driverName)) transporterConfig merchantId
              whenJust onboardingVehicleCategory $ \category -> do
                DIIQuery.updateOnboardingVehicleCategory (Just category) personId
        -- Check vehicle enablement separately (only vehicle docs + vehicle inspection)
        getVehicleDocuments allDocumentVerificationConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification True driverDocuments merchantOpCityId
      else do
        -- Combined enablement: Check both driver and vehicle docs together (old behavior)
        whenJust vehicleCategoryWithoutMandatoryConfigs $ \vehicleCategory -> do
          let allDriverDocsVerified = all (\doc -> checkIfDocumentValid allDocumentVerificationConfigs doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments
          when (allDriverDocsVerified && transporterConfig.requiresOnboardingInspection /= Just True && person.role == DP.DRIVER) $ do
            enableDriver merchantOpCityId personId (mDL >>= (.driverName)) transporterConfig merchantId
            whenJust onboardingVehicleCategory $ \category -> do
              DIIQuery.updateOnboardingVehicleCategory (Just category) personId
        -- Check vehicle enablement (old combined logic - checks both driver and vehicle docs)
        getVehicleDocuments allDocumentVerificationConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification False driverDocuments merchantOpCityId

  (dlDetails, rcDetails) <-
    case prefillData of
      Just True -> do
        let vehRegImgIds = map (.id) $ IQuery.filterImagesByPersonAndType driverImagesInfo merchantOperatingCity.merchantId DVC.VehicleRegistrationCertificate
        dl <- runInReplica $ DLQuery.findByDriverId personId <&> maybeToList
        allRCImgs <- runInReplica $ RCQuery.findAllByImageId vehRegImgIds
        allDLDetails <- mapM convertDLToDLDetails dl
        allRCDetails <- mapM convertRCToRCDetails allRCImgs
        return (Just allDLDetails, Just allRCDetails)
      _ -> return (Nothing, Nothing)

  enabled <- case person.role of
    DP.FLEET_OWNER -> do
      fleetOwnerInfo <- runInReplica $ QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      return fleetOwnerInfo.enabled
    _ -> do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
      return driverInfo.enabled

  digilockerResponseCode <- getDigilockerResponseCode personId

  digilockerAuthorizationUrl <-
    if transporterConfig.digilockerEnabled == Just True
      then SDDigilocker.getDigiLockerAuthorizationUrl personId
      else pure Nothing

  return $
    StatusRes'
      { driverDocuments,
        vehicleDocuments,
        enabled = enabled,
        manualVerificationRequired = transporterConfig.requiresOnboardingInspection,
        driverLicenseDetails = dlDetails,
        vehicleRegistrationCertificateDetails = rcDetails,
        digilockerResponseCode = digilockerResponseCode,
        digilockerAuthorizationUrl = digilockerAuthorizationUrl
      }
  where
    getVehicleDocuments allDocumentVerificationConfigs role vehicleDocumentsUnverified requiresOnboardingInspection vehicleCategoryExcludedFromVerification separateEnablement driverDocuments merchantOpCityId = do
      let personId = person.id
      vehicleDocumentsUnverified `forM` \vehicleDoc@VehicleDocumentItem {..} -> do
        let allVehicleDocsVerified = checkAllVehicleDocsVerified allDocumentVerificationConfigs vehicleDoc makeSelfieAadhaarPanMandatory
            inspectionNotRequired = requiresOnboardingInspection /= Just True || vehicleDoc.isApproved
            isVehicleCategoryExcludedFromVerification = (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) `elem` (fromMaybe [] vehicleCategoryExcludedFromVerification)
            -- When separated: only check vehicle docs. When combined: check both driver and vehicle docs
            allDriverDocsVerified = if separateEnablement then True else checkAllDriverDocsVerified allDocumentVerificationConfigs driverDocuments (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) makeSelfieAadhaarPanMandatory
            -- Vehicle activation logic depends on enablement mode
            checkToActivateRC =
              if separateEnablement
                then (allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification
                else ((allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification) && allDriverDocsVerified

        -- Activate RC if vehicle docs are verified and inspection is not required/approved
        mbVehicle <- QVehicle.findById personId
        when (shouldActivateRc && isNothing mbVehicle && checkToActivateRC && role == DP.DRIVER) $ do
          void $ withTryCatch "activateRCAutomatically:statusHandler" (activateRCAutomatically personId driverImagesInfo.merchantOperatingCity vehicleDoc.registrationNo)
          -- Enable driver when RC is activated (only when flow is NOT separated)
          -- When separated, driver enablement is handled separately in the driver enablement section
          unless separateEnablement $ do
            when checkToActivateRC $ do
              case (isVehicleCategoryExcludedFromVerification, mDL) of
                (True, _) -> enableDriver merchantOpCityId personId Nothing driverImagesInfo.transporterConfig driverImagesInfo.merchantOperatingCity.merchantId
                (False, Just dl) -> enableDriver merchantOpCityId personId dl.driverName driverImagesInfo.transporterConfig driverImagesInfo.merchantOperatingCity.merchantId
                (_, _) -> return ()
        if allVehicleDocsVerified then return VehicleDocumentItem {isVerified = True, ..} else return vehicleDoc

    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      let images = driverImagesInfo.driverImages
          mbImage1 = find (\img -> img.id == dl.documentImageId1) images
          mbImage2 = dl.documentImageId2 >>= \imgId2 -> find (\img -> img.id == imgId2) images
          s3Path1 = mbImage1 <&> (.s3Path)
          s3Path2 = mbImage2 <&> (.s3Path)
      pure $
        DLDetails
          { driverName = dl.driverName,
            driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show driverImagesInfo.merchantOperatingCity.city,
            driverDateOfBirth = dl.driverDob,
            classOfVehicles = dl.classOfVehicles,
            imageId1 = dl.documentImageId1.getId,
            imageId2 = getId <$> dl.documentImageId2,
            createdAt = dl.createdAt,
            dateOfIssue = dl.dateOfIssue,
            s3Path1 = s3Path1,
            s3Path2 = s3Path2,
            documentExpiry = Just dl.licenseExpiry
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
      let mbRcImage = find (\img -> img.id == rc.documentImageId) driverImagesInfo.driverImages
          s3Path = mbRcImage <&> (.s3Path)
      pure $
        RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show driverImagesInfo.merchantOperatingCity.city,
            vehicleCategory = show <$> rc.userPassedVehicleCategory,
            airConditioned = rc.airConditioned,
            vehicleManufacturer = rc.vehicleManufacturer,
            vehicleModel = rc.vehicleModel,
            vehicleColor = rc.vehicleColor,
            vehicleDoors = rc.vehicleDoors,
            vehicleSeatBelts = rc.vehicleSeatBelts,
            createdAt = rc.createdAt,
            dateOfRegistration = rc.dateOfRegistration,
            vehicleModelYear = rc.vehicleModelYear,
            oxygen = rc.oxygen,
            ventilator = rc.ventilator,
            failedRules = rc.failedRules,
            verificationStatus = Just rc.verificationStatus,
            s3Path = s3Path,
            documentExpiry = Just rc.fitnessExpiry -- Fitness expiry = RC expiry
          }

isFleetRole :: DP.Role -> Bool
isFleetRole DP.FLEET_OWNER = True
isFleetRole DP.FLEET_BUSINESS = True
isFleetRole _ = False

fetchDriverDocuments ::
  IQuery.DriverImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  [DVC.VehicleCategory] ->
  DP.Person ->
  Language ->
  Maybe Bool ->
  Maybe Bool ->
  Flow [DocumentStatusItem]
fetchDriverDocuments driverImagesInfo allDocumentVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs = do
  let role = person.role
      merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      driverId = person.id
      transporterConfig = driverImagesInfo.transporterConfig
      isDigiLockerEnabled = fromMaybe False transporterConfig.digilockerEnabled

  digilockerDocStatusMap <- if isDigiLockerEnabled then getDigilockerDocStatusMap driverId else pure DocStatus.emptyDocStatusMap

  driverDocumentTypes <- getDriverDocTypes merchantOpCityId allDocumentVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs
  driverDocumentTypes `forM` \docType -> do
    let mbDocStatus = if isDigiLockerEnabled then DocStatus.getDocStatus docType digilockerDocStatusMap else Nothing
        responseCode = mbDocStatus >>= (.responseCode)
        mbDocVerificationStatus = mbDocStatus >>= (mapDigilockerToResponseStatus . (.status))

    (mbProcessedStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path) <- getProcessedDriverDocuments person.id driverImagesInfo docType useHVSdkForDL
    (status, mbReason, mbUrl, mbExpiryFinal, mbS3PathFinal) <- case mbProcessedStatus of
      Just VALID -> pure (VALID, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path)
      Just s -> pure (s, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path)
      Nothing -> case mbDocVerificationStatus of
        Just docStatus -> pure (docStatus, Nothing, Nothing, Nothing, Nothing)
        Nothing -> getInProgressDriverDocuments driverId driverImagesInfo docType

    message <- documentStatusMessage status mbReason docType mbUrl language
    let finalMessage = mbReason <|> (if isDigiLockerEnabled then responseCode else Nothing) <|> Just message
    return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = finalMessage, verificationUrl = mbUrl, s3Path = mbS3PathFinal, documentExpiry = mbExpiryFinal}

fetchVehicleDocuments ::
  IQuery.DriverImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  Maybe Text ->
  Maybe Bool ->
  Flow [VehicleDocumentItem]
fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language Nothing onlyMandatoryDocs = do
  -- All items required
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC driverImagesInfo allDocumentVerificationConfigs language Nothing onlyMandatoryDocs
  processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC Nothing onlyMandatoryDocs
  let processedVehicleDocuments = processedVehicleDocumentsWithoutRC <> processedVehicleDocumentsWithRC
  inprogressVehicleDocuments <- fetchInprogressVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language processedVehicleDocuments Nothing onlyMandatoryDocs
  pure $ processedVehicleDocuments <> inprogressVehicleDocuments
fetchVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs = do
  -- Only one item required with specific registrationNo
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC driverImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs
  let driverId = driverImagesInfo.driverId
  if null processedVehicleDocumentsWithRC
    then do
      processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC (Just reqRegistrationNo) onlyMandatoryDocs
      if null processedVehicleDocumentsWithoutRC
        then do
          docs <- fetchInprogressVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language [] (Just reqRegistrationNo) onlyMandatoryDocs
          if null docs
            then logWarning $ "No docs found for rcNo and driverId: " <> show driverId
            else logInfo $ "Inprogress vehicle docs found for rcNo and driverId: " <> show driverId
          pure docs
        else do
          logInfo $ "Processed vehicle docs without RC found for rcNo and driverId: " <> show driverId
          pure processedVehicleDocumentsWithoutRC
    else do
      logInfo $ "Processed vehicle documents with RC found for rcNo and driverId: " <> show driverId
      pure processedVehicleDocumentsWithRC

getVehicleDocTypes ::
  (Monad m, Log m) =>
  Id DMOC.MerchantOperatingCity ->
  [DVC.DocumentVerificationConfig] ->
  Maybe DVC.VehicleCategory ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  m [DVC.DocumentType]
getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs = do
  let vehicleCategory = fromMaybe userSelectedVehicleCategory verifiedVehicleCategory
  let mandatoryVehicleDocumentVerificationConfigs = filter (\config -> fromMaybe config.isMandatory config.isMandatoryForEnabling && config.vehicleCategory == vehicleCategory) allDocumentVerificationConfigs
  if onlyMandatoryDocs == Just True
    then do
      let vehicleDocumentTypes = filter (\doc -> doc `elem` (mandatoryVehicleDocumentVerificationConfigs <&> (.documentType))) SDO.defaultVehicleDocumentTypes
      logInfo $
        "Fetch only mandatory vehicle docs types: merchantOpCityId: "
          <> merchantOpCityId.getId
          <> "; vehicleCategory: "
          <> show vehicleCategory
          <> "; vehicleDocumentTypes: "
          <> show vehicleDocumentTypes
      pure vehicleDocumentTypes
    else pure SDO.defaultVehicleDocumentTypes

getDriverDocTypes ::
  (Monad m, Log m) =>
  Id DMOC.MerchantOperatingCity ->
  [DVC.DocumentVerificationConfig] ->
  [DVC.VehicleCategory] ->
  DP.Role ->
  Maybe Bool ->
  m [DVC.DocumentType]
getDriverDocTypes merchantOpCityId allDocumentVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs = do
  if isFleetRole role
    then pure SDO.defaultFleetDocumentTypes
    else do
      let mandatoryVehicleDocumentVerificationConfigs = filter (\config -> fromMaybe config.isMandatory config.isMandatoryForEnabling && config.vehicleCategory `elem` possibleVehicleCategories) allDocumentVerificationConfigs
      if onlyMandatoryDocs == Just True
        then do
          let driverDocumentTypes = filter (\doc -> doc `elem` nub (mandatoryVehicleDocumentVerificationConfigs <&> (.documentType))) SDO.defaultDriverDocumentTypes
          logInfo $
            "Fetch only mandatory driver docs types: merchantOpCityId: "
              <> merchantOpCityId.getId
              <> "; possibleVehicleCategories: "
              <> show possibleVehicleCategories
              <> "; driverDocumentTypes: "
              <> show driverDocumentTypes
          pure driverDocumentTypes
        else pure SDO.defaultDriverDocumentTypes

fetchProcessedVehicleDocumentsWithRC ::
  IQuery.DriverImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  Maybe Text ->
  Maybe Bool ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithRC driverImagesInfo allDocumentVerificationConfigs language mbReqRegistrationNo onlyMandatoryDocs = do
  let mbPersonId = driverImagesInfo.driverId
      merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
  processedVehicles <- case mbPersonId of
    Just personId -> do
      associations <- DRAQuery.findAllLinkedByDriverId personId
      (catMaybes <$>) $
        forM associations $ \assoc -> do
          mbRc <- RCQuery.findById assoc.rcId
          -- filter by rcNo if required
          mbFilteredRc <- case mbRc of
            Just rc -> do
              rcCertificateNumber <- decrypt rc.certificateNumber
              let wrongRcNo = isJust mbReqRegistrationNo && Just rcCertificateNumber /= mbReqRegistrationNo
              return if wrongRcNo then Nothing else Just rc
            Nothing -> return Nothing
          return $ (assoc.isRcActive,assoc.rcId,) <$> mbFilteredRc
    Nothing ->
      case mbReqRegistrationNo of
        Nothing -> pure []
        Just reqRegistrationNo -> do
          -- fetch vehicle separately from driver
          mbRc <- RCQuery.findLastVehicleRCWrapper reqRegistrationNo
          case mbRc of
            Nothing -> pure []
            Just rc -> do
              mbAssoc <- DRAQuery.findLatestLinkedByRCId rc.id driverImagesInfo.now
              pure [(maybe False (.isRcActive) mbAssoc, rc.id, rc)]
  processedVehicles `forM` \(isActive, rcId, processedVehicle) -> do
    rcImages <- IQuery.findRecentByRcIdAndImageTypes driverImagesInfo.transporterConfig rcId vehicleDocsByRcIdList
    let rcImagesInfo = IQuery.RcImagesInfo {rcId, rcImages, documentTypes = vehicleDocsByRcIdList}
    registrationNo <- decrypt processedVehicle.certificateNumber
    let dateOfUpload = processedVehicle.createdAt
    let verifiedVehicleCategory = DV.castVehicleVariantToVehicleCategory <$> processedVehicle.vehicleVariant
        userSelectedVehicleCategory = fromMaybe DVC.CAR $ processedVehicle.userPassedVehicleCategory <|> verifiedVehicleCategory

    vehicleDocumentTypes <- getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs
    documents <-
      vehicleDocumentTypes `forM` \docType -> do
        (mbStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path) <- getProcessedVehicleDocuments driverImagesInfo docType processedVehicle (Just rcImagesInfo)
        case mbStatus of
          Just status -> do
            message <- documentStatusMessage status Nothing docType mbProcessedUrl language
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl, s3Path = mbS3Path, documentExpiry = mbExpiry}
          Nothing -> do
            (status, mbReason, mbUrl, _, mbS3PathInProgress) <- getInProgressVehicleDocuments driverImagesInfo (Just rcImagesInfo) docType
            message <- documentStatusMessage status mbReason docType mbUrl language
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl, s3Path = mbS3PathInProgress, documentExpiry = Nothing}

    let mbRcImage = find (\img -> img.id == processedVehicle.documentImageId) driverImagesInfo.driverImages
        rcS3Path = mbRcImage <&> (.s3Path)
        rcExpiry = Just processedVehicle.fitnessExpiry
    return
      VehicleDocumentItem
        { registrationNo,
          userSelectedVehicleCategory,
          verifiedVehicleCategory,
          isVerified = False,
          isActive,
          isApproved = fromMaybe False processedVehicle.approved,
          vehicleModel = processedVehicle.vehicleModel,
          documents,
          dateOfUpload,
          s3Path = rcS3Path,
          documentExpiry = rcExpiry
        }

fetchProcessedVehicleDocumentsWithoutRC ::
  IQuery.DriverImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Maybe Bool ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC mbReqRegistrationNo onlyMandatoryDocs = do
  let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      mbPersonId = driverImagesInfo.driverId
  mbVehicle <- maybe (pure Nothing) QVehicle.findById mbPersonId
  case mbVehicle of
    Just vehicle -> do
      let vehicleAlreadyIncluded = isJust $ find (\doc -> doc.registrationNo == vehicle.registrationNo) processedVehicleDocumentsWithRC
      -- filter by rcNo if required
      let wrongRcNo = isJust mbReqRegistrationNo && Just vehicle.registrationNo /= mbReqRegistrationNo
      if vehicleAlreadyIncluded || wrongRcNo
        then return []
        else do
          let userSelectedVehicleCategory = DV.castVehicleVariantToVehicleCategory vehicle.variant
              verifiedVehicleCategory = Just $ DV.castVehicleVariantToVehicleCategory vehicle.variant
          vehicleDocumentTypes <- getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs

          documents <-
            vehicleDocumentTypes `forM` \docType -> do
              return $ DocumentStatusItem {documentType = docType, verificationStatus = NO_DOC_AVAILABLE, verificationMessage = Nothing, verificationUrl = Nothing, s3Path = Nothing, documentExpiry = Nothing}
          return
            [ VehicleDocumentItem
                { registrationNo = vehicle.registrationNo,
                  userSelectedVehicleCategory,
                  verifiedVehicleCategory,
                  isVerified = True,
                  isActive = True,
                  isApproved = False,
                  vehicleModel = Just vehicle.model,
                  documents,
                  dateOfUpload = vehicle.createdAt,
                  s3Path = Nothing,
                  documentExpiry = Nothing
                }
            ]
    Nothing -> return []

fetchInprogressVehicleDocuments ::
  IQuery.DriverImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Maybe Bool ->
  Flow [VehicleDocumentItem]
fetchInprogressVehicleDocuments driverImagesInfo allDocumentVerificationConfigs language processedVehicleDocuments mbReqRegistrationNo onlyMandatoryDocs = do
  let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      mbPersonId = driverImagesInfo.driverId
  mbVerificationReqRecord <- case mbPersonId of
    Just personId -> do
      inprogressVehicleIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing personId DVC.VehicleRegistrationCertificate
      inprogressVehicleHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing personId DVC.VehicleRegistrationCertificate
      pure $ getLatestVerificationRecord inprogressVehicleIdfy inprogressVehicleHV
    Nothing -> pure Nothing
  case mbVerificationReqRecord of
    Just verificationReqRecord -> do
      registrationNoEither <- withTryCatch "decryptDocumentNumber:fetchInprogressVehicleDocuments" (decrypt verificationReqRecord.documentNumber)
      case registrationNoEither of
        Left err -> do
          logError $ "Error while decrypting document number: " <> (verificationReqRecord.documentNumber & unEncrypted . encrypted) <> " with err: " <> show err
          return []
        Right registrationNo -> do
          -- filter by rcNo if required
          let wrongRcNo = isJust mbReqRegistrationNo && Just registrationNo /= mbReqRegistrationNo
          if wrongRcNo
            then return []
            else do
              rcNoEnc <- encrypt registrationNo
              rc <- RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
              (isUnlinked, mbRcId) <- case rc of
                Just rc_ -> do
                  iu <- maybe (pure []) (\personId -> DRAQuery.findUnlinkedRC personId rc_.id) mbPersonId
                  pure (iu, Just rc_.id)
                Nothing -> pure ([], Nothing)
              mbRcImagesInfo <- forM mbRcId $ \rcId -> do
                rcImages <- IQuery.findRecentByRcIdAndImageTypes driverImagesInfo.transporterConfig rcId vehicleDocsByRcIdList
                pure IQuery.RcImagesInfo {rcId, rcImages, documentTypes = vehicleDocsByRcIdList}
              if isJust (find (\doc -> doc.registrationNo == registrationNo) processedVehicleDocuments) || not (null isUnlinked)
                then return []
                else do
                  let userSelectedVehicleCategory = fromMaybe DVC.CAR verificationReqRecord.vehicleCategory
                      verifiedVehicleCategory = Nothing

                  vehicleDocumentTypes <- getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs
                  documents <-
                    vehicleDocumentTypes `forM` \docType -> do
                      (status, mbReason, mbUrl, _, mbS3Path) <- getInProgressVehicleDocuments driverImagesInfo mbRcImagesInfo docType
                      message <- documentStatusMessage status mbReason docType mbUrl language
                      return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl, s3Path = mbS3Path, documentExpiry = Nothing}
                  let mbRcImage = find (\img -> img.imageType == DVC.VehicleRegistrationCertificate) driverImagesInfo.driverImages
                      rcS3Path = mbRcImage <&> (.s3Path)
                  return
                    [ VehicleDocumentItem
                        { registrationNo,
                          userSelectedVehicleCategory,
                          verifiedVehicleCategory,
                          isVerified = False,
                          isActive = False,
                          isApproved = False,
                          vehicleModel = Nothing,
                          documents,
                          dateOfUpload = verificationReqRecord.createdAt,
                          s3Path = rcS3Path,
                          documentExpiry = Nothing
                        }
                    ]
    Nothing -> return []

checkAllVehicleDocsVerified ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsVerified allDocumentVerificationConfigs vehicleDoc makeSelfieAadhaarPanMandatory = do
  all (\doc -> checkIfDocumentValid allDocumentVerificationConfigs doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus makeSelfieAadhaarPanMandatory) vehicleDoc.documents

checkAllDriverDocsVerified ::
  [DVC.DocumentVerificationConfig] ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsVerified allDocumentVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory = do
  all (\doc -> checkIfDocumentValid allDocumentVerificationConfigs doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments

enableDriver :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> Maybe Text -> DTC.TransporterConfig -> Id DM.Merchant -> Flow ()
enableDriver merchantOpCityId personId driverName transporterConfig merchantId = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  unless driverInfo.enabled $ do
    SDO.enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId True
    whenJust driverName $ \name -> QPerson.updateName name personId
    -- Send SMS on enablement if configured
    when (transporterConfig.sendSmsOnEnablement == Just True) $ do
      fork "sending sms - onboarding" $ do
        driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
        smsCfg <- asks (.smsCfg)
        mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
        let countryCode = fromMaybe "+91" driver.mobileCountryCode
            phoneNumber = countryCode <> mobileNumber
            sender = smsCfg.sender
        (mbSender, message, templateId, messageType) <-
          MessageBuilder.buildOnboardingMessage merchantOpCityId $
            MessageBuilder.BuildOnboardingMessageReq {}
        Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber (fromMaybe sender mbSender) templateId messageType) >>= Sms.checkSmsResult

activateRCAutomatically :: Id DP.Person -> DMOC.MerchantOperatingCity -> Text -> Flow ()
activateRCAutomatically personId merchantOpCity rcNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantOpCity.merchantId, merchantOpCity.id) rcStatusReq

checkIfDocumentValid ::
  [DVC.DocumentVerificationConfig] ->
  DDVC.DocumentType ->
  DVC.VehicleCategory ->
  ResponseStatus ->
  Maybe Bool ->
  Bool
checkIfDocumentValid _allDocumentVerificationConfigs _docType _category VALID _makeSelfieAadhaarPanMandatory = True
checkIfDocumentValid allDocumentVerificationConfigs docType category status makeSelfieAadhaarPanMandatory = do
  let mbVerificationConfig = find (\config -> config.documentType == docType && config.vehicleCategory == category) allDocumentVerificationConfigs
  case mbVerificationConfig of
    Just verificationConfig -> do
      if fromMaybe verificationConfig.isMandatory verificationConfig.isMandatoryForEnabling && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory)
        then case status of
          MANUAL_VERIFICATION_REQUIRED -> verificationConfig.isDefaultEnabledOnManualVerification
          _ -> False
        else True
    Nothing -> True

getProcessedDriverDocuments :: Id DP.Person -> IQuery.DriverImagesInfo -> DVC.DocumentType -> Maybe Bool -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text)
getProcessedDriverDocuments driverId driverImagesInfo docType useHVSdkForDL = do
  let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      mbS3Path = getS3PathFromLatestImage driverImagesInfo docType
  case docType of
    DVC.DriverLicense -> do
      mbDL <- DLQuery.findByDriverId driverId -- add failure reason in dl and rc
      if isNothing mbDL && (useHVSdkForDL == Just True)
        then do
          void $ withTryCatch "callGetDLGetStatus:getProcessedDriverDocuments" $ callGetDLGetStatus driverId merchantOpCityId
          mbDL' <- DLQuery.findByDriverId driverId
          -- Expiry from DL table's licenseExpiry field (not from Image table)
          return (mapStatus <$> (mbDL' <&> (.verificationStatus)), mbDL' >>= (.rejectReason), Nothing, mbDL' <&> (.licenseExpiry), mbS3Path)
        else return (mapStatus <$> (mbDL <&> (.verificationStatus)), mbDL >>= (.rejectReason), Nothing, mbDL <&> (.licenseExpiry), mbS3Path)
    DVC.AadhaarCard -> do
      mbAadhaarCard <- QAadhaarCard.findByPrimaryKey driverId
      return (mapStatus . (.verificationStatus) <$> mbAadhaarCard, Nothing, Nothing, Nothing, mbS3Path)
    DVC.Permissions -> return (Just VALID, Nothing, Nothing, Nothing, mbS3Path)
    DVC.SocialSecurityNumber -> do
      mbSSN <- QDSSN.findByDriverId driverId
      return (mapStatus <$> (mbSSN <&> (.verificationStatus)), mbSSN >>= (.rejectReason), Nothing, Nothing, mbS3Path)
    DVC.ProfilePhoto -> do
      let (status, reason, url) = checkImageValidity driverImagesInfo DVC.ProfilePhoto
      return (status, reason, url, Nothing, mbS3Path)
    DVC.UploadProfile -> do
      let (status, reason, url) = checkImageValidity driverImagesInfo DVC.UploadProfile
      return (status, reason, url, Nothing, mbS3Path)
    DVC.PanCard -> do
      mbPanCard <- QDPC.findByDriverId driverId
      return (mapStatus . (.verificationStatus) <$> mbPanCard, Nothing, Nothing, Nothing, mbS3Path)
    DVC.GSTCertificate -> do
      mbGSTCertificate <- QDGST.findByDriverId driverId
      return (mapStatus . (.verificationStatus) <$> mbGSTCertificate, Nothing, Nothing, Nothing, mbS3Path)
    DVC.BackgroundVerification -> do
      mbBackgroundVerification <- BVQuery.findByDriverId driverId
      -- Expiry from BackgroundVerification table's expiresAt field (not from Image table)
      if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
        then return (Just VALID, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path)
        else return (Nothing, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path)
    DVC.DrivingSchoolCertificate -> do
      let (status, reason, url) = checkImageValidity driverImagesInfo DVC.DrivingSchoolCertificate
      return (status, reason, url, Nothing, mbS3Path)
    DVC.PoliceVerificationCertificate -> do
      let (status, reason, url) = checkImageValidity driverImagesInfo DVC.PoliceVerificationCertificate
      return (status, reason, url, Nothing, mbS3Path)
    DVC.LocalResidenceProof -> do
      let (status, reason, url) = checkImageValidity driverImagesInfo DVC.LocalResidenceProof
      return (status, reason, url, Nothing, mbS3Path)
    DVC.TrainingForm -> do
      status <- checkLMSTrainingStatus driverId merchantOpCityId
      return (status, Nothing, Nothing, Nothing, mbS3Path)
    DVC.DriverInspectionHub -> do
      status <- getInspectionHubStatusForResponseStatus DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
      return (status, Nothing, Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing, mbS3Path)

callGetDLGetStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Flow ()
callGetDLGetStatus driverId merchantOpCityId = do
  latestReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId DVC.DriverLicense
  whenJust latestReq $ \verificationReq -> do
    when (verificationReq.status == "pending" || verificationReq.status == "source_down_retrying") $ do
      rsp <- Verification.getTask merchantOpCityId KEV.HyperVergeRCDL (KEV.GetTaskReq (Just "checkDL") verificationReq.requestId) HVQuery.updateResponse
      case rsp of
        KEV.DLResp resp -> do
          logDebug $ "callGetDLGetStatus: getTask api response for request id : " <> verificationReq.requestId <> " is : " <> show resp
          unless ("still being processed" `T.isInfixOf` (fromMaybe "" resp.message)) (void $ DDL.onVerifyDL (SDO.makeHVVerificationReqRecord verificationReq) resp KEV.HyperVergeRCDL)
        _ -> throwError $ InternalError "Document and apiEndpoint mismatch occurred !!!!!!!!"

getProcessedVehicleDocuments :: IQuery.DriverImagesInfo -> DVC.DocumentType -> RC.VehicleRegistrationCertificate -> Maybe IQuery.RcImagesInfo -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text)
getProcessedVehicleDocuments driverImagesInfo docType vehicleRC mbRcImagesInfo = do
  let mbDriverId = driverImagesInfo.driverId
      mbS3Path = getS3PathFromVehicleImage driverImagesInfo docType mbRcImagesInfo
  case docType of
    DVC.VehicleRegistrationCertificate ->
      return (Just $ mapStatus vehicleRC.verificationStatus, vehicleRC.rejectReason, Nothing, Just vehicleRC.fitnessExpiry, mbS3Path)
    DVC.VehiclePermit -> do
      mbDoc <- case mbDriverId of
        Just driverId -> listToMaybe <$> VPQuery.findByRcIdAndDriverId vehicleRC.id driverId
        Nothing -> listToMaybe <$> VPQuery.findByRcId vehicleRC.id
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, vehicleRC.permitExpiry, mbS3Path)
    DVC.VehicleFitnessCertificate -> do
      mbDoc <- case mbDriverId of
        Just driverId -> listToMaybe <$> VFCQuery.findByRcIdAndDriverId vehicleRC.id driverId
        Nothing -> listToMaybe <$> VFCQuery.findByRcId vehicleRC.id
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, Just vehicleRC.fitnessExpiry, mbS3Path)
    DVC.VehicleInsurance -> do
      mbDoc <- case mbDriverId of
        Just driverId -> listToMaybe <$> VIQuery.findByRcIdAndDriverId vehicleRC.id driverId
        Nothing -> listToMaybe <$> VIQuery.findByRcId vehicleRC.id
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), (mbDoc >>= (.rejectReason)), Nothing, vehicleRC.insuranceValidity, mbS3Path)
    DVC.VehiclePUC -> do
      mbDoc <- case mbDriverId of
        Just driverId -> listToMaybe <$> VPUCQuery.findByRcIdAndDriverId vehicleRC.id driverId
        Nothing -> listToMaybe <$> VPUCQuery.findByRcId vehicleRC.id
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, vehicleRC.pucExpiry, mbS3Path)
    DVC.VehicleNOC -> do
      mbDoc <- case mbDriverId of
        Just driverId -> listToMaybe <$> VNOCQuery.findByRcIdAndDriverId vehicleRC.id driverId
        Nothing -> listToMaybe <$> VNOCQuery.findByRcId vehicleRC.id
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, mbDoc <&> (.nocExpiry), mbS3Path)
    DVC.VehicleInspectionForm -> do
      -- Check all vehicle photos based on RC, not driver
      (status, reason, url) <- checkVehiclePhotosStatusByRC mbRcImagesInfo
      return (Just status, reason, url, Nothing, Nothing)
    DVC.InspectionHub -> do
      registrationNo <- decrypt vehicleRC.certificateNumber
      status <- getInspectionHubStatusForResponseStatus DOHR.ONBOARDING_INSPECTION Nothing (Just registrationNo)
      return (status, Nothing, Nothing, Nothing, Nothing)
    DVC.SubscriptionPlan -> case mbDriverId of
      Just driverId -> do
        mbPlan <- snd <$> DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION driverId -- fix later on basis of vehicle category
        return (Just $ boolToStatus (isJust mbPlan), Nothing, Nothing, Nothing, Nothing)
      Nothing -> return (Nothing, Nothing, Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing, Nothing, mbS3Path)
  where
    boolToStatus :: Bool -> ResponseStatus
    boolToStatus = \case
      True -> VALID
      False -> NO_DOC_AVAILABLE

getS3PathFromLatestImage :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Maybe Text
getS3PathFromLatestImage driverImagesInfo docType =
  let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
      mbLatestImage = listToMaybe images
   in mbLatestImage <&> (.s3Path)

getS3PathFromVehicleImage :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Maybe IQuery.RcImagesInfo -> Maybe Text
getS3PathFromVehicleImage driverImagesInfo docType mbRcImagesInfo =
  let images = case docType of
        DVC.VehicleRegistrationCertificate ->
          let merchantId = driverImagesInfo.merchantOperatingCity.merchantId
           in IQuery.filterImagesByPersonAndType driverImagesInfo merchantId docType
        _ -> case mbRcImagesInfo of
          Just rcImagesInfo -> IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
          Nothing -> []
      mbLatestImage = listToMaybe images
   in mbLatestImage <&> (.s3Path)

checkImageValidity :: IQuery.DriverImagesInfo -> DVC.DocumentType -> (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
checkImageValidity driverImagesInfo docType = do
  let validImages = IQuery.filterImageByPersonIdAndImageTypeAndVerificationStatus driverImagesInfo docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == Just Documents.VALID) validImages = (Just VALID, Nothing, Nothing)
      | any (\img -> img.verificationStatus == Just Documents.MANUAL_VERIFICATION_REQUIRED) validImages = (Just MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
      | otherwise = (Nothing, Nothing, Nothing)

checkLMSTrainingStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Flow (Maybe ResponseStatus)
checkLMSTrainingStatus driverId merchantOpCityId = do
  -- Get Plasma service config from merchant service config
  mbPlasmaConfig <- QMSC.findByServiceAndCity (DMSC.PlasmaService Plasma.LMS) merchantOpCityId
  case mbPlasmaConfig of
    Nothing -> return Nothing -- No Plasma config found, return Nothing
    Just plasmaServiceConfig -> do
      case plasmaServiceConfig.serviceConfig of
        DMSC.PlasmaServiceConfig plasmaConfig -> do
          -- Call Plasma LMS API to get modules for the driver
          -- getLMSModules takes PlasmaServiceConfig and driverId as Text
          mbModules <- withTryCatch "checkLMSTrainingStatus" $ do
            Plasma.getLMSModules plasmaConfig (driverId.getId)
          case mbModules of
            Right modules -> do
              -- Check if all modules have status COMPLETED
              let hasCompleted = all (\lmsModule -> lmsModule.status == Plasma.COMPLETED) modules
              return $ if hasCompleted then Just VALID else Nothing
            Left _ -> return Nothing -- API call failed, return Nothing
        _ -> return Nothing -- Invalid service config type

checkVehiclePhotosStatusByRC :: Maybe IQuery.RcImagesInfo -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkVehiclePhotosStatusByRC mbRcImagesInfo = do
  -- Check all vehicle photo types from RC images (6 types: VehicleLeft, VehicleRight, VehicleFrontInterior, VehicleBackInterior, VehicleFront, VehicleBack)
  let vehiclePhotoTypes = vehicleDocsByRcIdList
      photoStatuses = case mbRcImagesInfo of
        Just rcImagesInfo ->
          mapMaybe
            ( \docType -> do
                let images = IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
                case images of
                  [] -> Nothing -- No image for this type
                  img : _ -> Just img.verificationStatus -- Get status of most recent image for this type
            )
            vehiclePhotoTypes
        Nothing -> []
  case photoStatuses of
    [] -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    statuses -> do
      -- Check if all photos exist (must have all 6)
      let allPhotosExist = length statuses == length vehiclePhotoTypes
          allValid = allPhotosExist && all (== Just Documents.VALID) statuses
          allManualVerification = allPhotosExist && all (== Just Documents.MANUAL_VERIFICATION_REQUIRED) statuses
          -- Check if we have some VALID and all others are MANUAL_VERIFICATION_REQUIRED (no INVALID or missing)
          hasValid = any (== Just Documents.VALID) statuses
          onlyValidOrManual = all (\s -> s == Just Documents.VALID || s == Just Documents.MANUAL_VERIFICATION_REQUIRED) statuses
          someValidAndRestManual = allPhotosExist && hasValid && onlyValidOrManual && not allValid
      if allValid
        then return (VALID, Nothing, Nothing)
        else
          if allManualVerification || someValidAndRestManual
            then return (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
            else return (INVALID, Nothing, Nothing)

checkBackgroundVerificationStatus :: Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkBackgroundVerificationStatus driverId merchantId merchantOpCityId = do
  mbBackgroundVerification <- BVQuery.findByDriverId driverId
  case mbBackgroundVerification of
    Just backgroundVerification -> do
      now <- getCurrentTime
      if now >= backgroundVerification.expiresAt
        then return (NO_DOC_AVAILABLE, Nothing, Nothing)
        else do
          invitation <- BackgroundVerification.getInvitation merchantId merchantOpCityId backgroundVerification.invitationId
          case invitation.status of
            "completed" -> do
              BVQuery.updateInvitationStatus Documents.VALID driverId
              case invitation.reportId of
                Just reportId -> do
                  BVQuery.updateReportId (Just reportId) driverId
                  report <- BackgroundVerification.getReport merchantId merchantOpCityId reportId
                  case report.status of
                    "complete" -> do
                      case (report.assessment, report.adjudication) of
                        (Just "eligible", _) -> do
                          BVQuery.updateReportStatus Documents.VALID driverId
                          return (VALID, Nothing, Nothing)
                        (_, Just "engaged") -> do
                          BVQuery.updateReportStatus Documents.VALID driverId
                          return (VALID, Nothing, Nothing)
                        (_, Just "post_adverse_action") -> do
                          BVQuery.updateReportStatus Documents.UNAUTHORIZED driverId
                          return (UNAUTHORIZED, Nothing, Nothing)
                        (_, _) -> return (PENDING, Nothing, Nothing)
                    "pending" -> return (PENDING, Nothing, Nothing)
                    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
                Nothing -> return (PENDING, Nothing, Nothing)
            "pending" -> return (PENDING, Nothing, Just invitation.invitationUrl)
            _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

getInProgressDriverDocuments ::
  Id DP.Person ->
  IQuery.DriverImagesInfo ->
  DDVC.DocumentType ->
  Flow (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text)
getInProgressDriverDocuments driverId driverImagesInfo docType = do
  let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      merchantId = driverImagesInfo.merchantOperatingCity.merchantId
      mbS3Path = getS3PathFromLatestImage driverImagesInfo docType
  (status, mbReason, mbUrl) <- case docType of
    DDVC.DriverLicense -> checkIfUnderProgress driverImagesInfo DDVC.DriverLicense
    DDVC.BackgroundVerification -> checkBackgroundVerificationStatus driverId merchantId merchantOpCityId
    DDVC.AadhaarCard -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.AadhaarCard
    DDVC.PanCard -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.PanCard
    DDVC.GSTCertificate -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.GSTCertificate
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      let mbImages = IQuery.filterRecentLatestByPersonIdAndImageType driverImagesInfo DDVC.ProfilePhoto
      return (fromMaybe NO_DOC_AVAILABLE (mapStatus <$> (mbImages >>= (.verificationStatus))), Nothing, Nothing)
    DDVC.UploadProfile -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.UploadProfile
    DDVC.DrivingSchoolCertificate -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.DrivingSchoolCertificate
    DDVC.PoliceVerificationCertificate -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.PoliceVerificationCertificate
    DDVC.LocalResidenceProof -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.LocalResidenceProof
    DDVC.TrainingForm -> checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.TrainingForm
    DDVC.DriverInspectionHub -> do
      mbStatus <- getInspectionHubStatusForResponseStatus DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
      let status = fromMaybe INVALID mbStatus
      return (status, Nothing, Nothing)
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
  return (status, mbReason, mbUrl, Nothing, mbS3Path)

vehicleDocsByRcIdList :: [DVC.DocumentType]
vehicleDocsByRcIdList =
  [ DVC.VehicleLeft,
    DVC.VehicleRight,
    DVC.VehicleFrontInterior,
    DVC.VehicleBackInterior,
    DVC.VehicleFront,
    DVC.VehicleBack,
    DVC.Odometer
  ]

getInProgressVehicleDocuments :: IQuery.DriverImagesInfo -> Maybe IQuery.RcImagesInfo -> DVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text)
getInProgressVehicleDocuments driverImagesInfo mbRcImagesInfo docType = do
  let mbS3Path = case mbRcImagesInfo of
        Just rcImagesInfo ->
          let images = IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
              mbLatestImage = listToMaybe images
           in mbLatestImage <&> (.s3Path)
        Nothing -> getS3PathFromLatestImage driverImagesInfo docType
  (status, mbReason, mbUrl) <- case docType of
    DVC.VehicleRegistrationCertificate -> checkIfUnderProgress driverImagesInfo DVC.VehicleRegistrationCertificate
    DVC.SubscriptionPlan -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    DVC.VehiclePermit -> checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehiclePermit
    DVC.VehicleFitnessCertificate -> checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleFitnessCertificate
    DVC.VehicleInsurance -> checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleInsurance
    DVC.VehiclePUC -> checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehiclePUC
    DVC.VehicleInspectionForm -> checkVehiclePhotosStatusByRC mbRcImagesInfo
    DVC.VehicleNOC -> checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleNOC
    DVC.InspectionHub -> do
      mbRegistrationNo <- case mbRcImagesInfo of
        Just rcImagesInfo -> do
          mbRc <- runInReplica $ RCQuery.findById rcImagesInfo.rcId
          case mbRc of
            Just rc -> Just <$> decrypt rc.certificateNumber
            Nothing -> return Nothing
        Nothing -> return Nothing
      case mbRegistrationNo of
        Just registrationNo -> do
          mbStatus <- getInspectionHubStatusForResponseStatus DOHR.ONBOARDING_INSPECTION Nothing (Just registrationNo)
          let status = fromMaybe INVALID mbStatus
          return (status, Nothing, Nothing)
        Nothing -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    _ | docType `elem` vehicleDocsByRcIdList -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo docType
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
  return (status, mbReason, mbUrl, Nothing, mbS3Path)

checkIfImageUploadedOrInvalidatedByRC :: Maybe IQuery.RcImagesInfo -> DDVC.DocumentType -> (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo docType = do
  let images = case mbRcImagesInfo of
        Just rcImagesInfo -> IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
        _ -> []
  case images of
    [] -> (NO_DOC_AVAILABLE, Nothing, Nothing)
    latestImage : _ -> do
      if latestImage.verificationStatus == Just Documents.INVALID
        then (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        else (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

checkIfImageUploadedOrInvalidated :: IQuery.DriverImagesInfo -> DDVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated driverImagesInfo docType = do
  let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
  documentVerificationConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndDefaultEnabledOnManualVerification driverImagesInfo.merchantOperatingCity.id docType False Nothing
  case images of
    [] -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    latestImage : _ -> do
      if latestImage.verificationStatus == Just Documents.INVALID
        then return (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        else
          if length documentVerificationConfig > 0
            then return (FAILED, Nothing, Nothing)
            else return (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

checkIfUnderProgress :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfUnderProgress driverImagesInfo docType = do
  let mbDriverId = driverImagesInfo.driverId
  mbVerificationReqRecord <- case mbDriverId of
    Just driverId -> do
      mbVerificationReqIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
      mbVerificationReqHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
      pure $ getLatestVerificationRecord mbVerificationReqIdfy mbVerificationReqHV
    Nothing -> pure Nothing
  case mbVerificationReqRecord of
    Just verificationReqRecord -> do
      if verificationReqRecord.status == "pending" || verificationReqRecord.status == "source_down_retrying"
        then return (PENDING, Nothing, Nothing)
        else return (FAILED, verificationReqRecord.verificaitonResponse, Nothing)
    Nothing -> do
      let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
      handleImages driverImagesInfo.transporterConfig.onboardingTryLimit images
  where
    handleImages onboardingTryLimit images
      | null images = return (NO_DOC_AVAILABLE, Nothing, Nothing)
      | length images > onboardingTryLimit * bool 1 2 (docType == DVC.DriverLicense || docType == DVC.AadhaarCard) = return (LIMIT_EXCEED, Nothing, Nothing)
      | otherwise = do
        let latestImage = head images
        if latestImage.verificationStatus == Just Documents.INVALID
          then return (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
          else return (NO_DOC_AVAILABLE, Nothing, Nothing)

extractImageFailReason :: Maybe DriverOnboardingError -> Maybe Text
extractImageFailReason imageError =
  case imageError of
    Just (ImageNotValid reason) -> Just reason -- only this because we are inserting this type only in manual reject request in update documents dashboard api
    _ -> Nothing

documentStatusMessage :: ResponseStatus -> Maybe Text -> DDVC.DocumentType -> Maybe BaseUrl -> Language -> Flow Text
documentStatusMessage status mbReason docType mbVerificationUrl language = do
  case (status, docType, mbVerificationUrl) of
    (VALID, _, _) -> toVerificationMessage DocumentValid language
    (MANUAL_VERIFICATION_REQUIRED, _, _) -> toVerificationMessage UnderManualReview language
    (PENDING, DDVC.BackgroundVerification, Just _) -> toVerificationMessage VerificationPendingOnUserInput language
    (PENDING, _, _) -> toVerificationMessage VerificationInProgress language
    (PULL_REQUIRED, _, _) -> toVerificationMessage PullRequired language
    (CONSENT_DENIED, _, _) -> toVerificationMessage ConsentDenied language
    (LIMIT_EXCEED, _, _) -> toVerificationMessage LimitExceed language
    (NO_DOC_AVAILABLE, _, _) -> toVerificationMessage NoDcoumentFound language
    (INVALID, DDVC.DriverLicense, _) -> do
      msg <- toVerificationMessage DLInvalid language
      return $ fromMaybe msg mbReason
    (INVALID, DDVC.VehicleRegistrationCertificate, _) -> do
      msg <- toVerificationMessage RCInvalid language
      return $ fromMaybe msg mbReason
    (INVALID, _, _) -> do
      msg <- toVerificationMessage DocumentInvalid language
      return $ fromMaybe msg mbReason
    (UNAUTHORIZED, _, _) -> toVerificationMessage Unauthorized language
    (FAILED, _, _) -> do
      case mbReason of
        Just res
          | "id_not_found" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
          | "source_down" `T.isInfixOf` res -> toVerificationMessage VerificationInProgress language
          | "TIMEOUT" `T.isInfixOf` res -> toVerificationMessage VerficationFailed language
          | "BAD_REQUEST" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
          | "422" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
          | otherwise -> toVerificationMessage Other language
        Nothing -> toVerificationMessage Other language

getAadhaarStatus :: Id DP.Person -> Flow (ResponseStatus, Maybe DAadhaarCard.AadhaarCard)
getAadhaarStatus personId = do
  mAadhaarCard <- QAadhaarCard.findByPrimaryKey personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.verificationStatus == Documents.VALID
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id DP.Person -> IQuery.DriverImagesInfo -> Language -> Maybe Bool -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverId driverImagesInfo language useHVSdkForDL = do
  let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
  mDriverLicense <- do
    mbDL' <- DLQuery.findByDriverId driverId
    case mbDL' of
      Just dl -> return $ Just dl
      Nothing -> do
        if useHVSdkForDL == Just True
          then do
            void $ withTryCatch "callGetDLGetStatus:getDLAndStatus" $ callGetDLGetStatus driverId merchantOpCityId
            DLQuery.findByDriverId driverId
          else return Nothing
  (status, message) <-
    case mDriverLicense of
      Just driverLicense -> do
        let status = mapStatus driverLicense.verificationStatus
        msg <- verificationStatusCheck status language DVC.DriverLicense Nothing
        return (status, msg)
      Nothing -> do
        (status, message) <- checkIfInVerification driverId driverImagesInfo DVC.DriverLicense language
        return (status, message)
  return (status, mDriverLicense, message)

getRCAndStatus :: Id DP.Person -> IQuery.DriverImagesInfo -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
getRCAndStatus driverId driverImagesInfo language = do
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      (status, message) <- checkIfInVerification driverId driverImagesInfo DVC.VehicleRegistrationCertificate language
      return (status, Nothing, message)
    else do
      mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
      let vehicleRCs = catMaybes mVehicleRCs
      let mValidVehicleRC = find (\rc -> rc.verificationStatus == Documents.VALID) vehicleRCs
      case mValidVehicleRC of
        Just validVehicleRC -> do
          msg <- toVerificationMessage DocumentValid language
          return (VALID, Just validVehicleRC, msg)
        Nothing -> do
          let mVehicleRC = listToMaybe vehicleRCs
          case mVehicleRC of
            Just vehicleRC -> do
              let status = mapStatus vehicleRC.verificationStatus
              message <- verificationStatusCheck status language DVC.VehicleRegistrationCertificate (Just vehicleRC.failedRules)
              return (status, Just vehicleRC, message)
            Nothing -> do
              msg <- toVerificationMessage NoDcoumentFound language
              return (NO_DOC_AVAILABLE, Nothing, msg)

mapStatus :: Documents.VerificationStatus -> ResponseStatus
mapStatus = \case
  Documents.PENDING -> PENDING
  Documents.MANUAL_VERIFICATION_REQUIRED -> MANUAL_VERIFICATION_REQUIRED
  Documents.VALID -> VALID
  Documents.INVALID -> INVALID
  Documents.UNAUTHORIZED -> UNAUTHORIZED
  Documents.PULL_REQUIRED -> PULL_REQUIRED

verificationStatusCheck :: ResponseStatus -> Language -> DVC.DocumentType -> Maybe [Text] -> Flow Text
verificationStatusCheck status language img mbReasons = do
  case (status, img) of
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> do
      msg <- toVerificationMessage RCInvalid language
      addVerificationReasons language mbReasons msg
    _ -> toVerificationMessage DocumentValid language

addVerificationReasons :: Language -> Maybe [Text] -> Text -> Flow Text
addVerificationReasons language mbReasons msg = do
  case mbReasons of
    Just reasons | not (null reasons) -> do
      translatedReasons <- forM reasons $ \reason -> do
        let (key, value) = T.breakOn ":" reason
        translatedKey <- translateDynamicKey key language
        if T.null value
          then pure translatedKey
          else pure $ translatedKey <> ": " <> T.drop 1 value
      pure $ msg <> T.intercalate ", " translatedReasons
    _ -> pure msg

checkIfInVerification :: Id DP.Person -> IQuery.DriverImagesInfo -> DVC.DocumentType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverId driverImagesInfo docType language = do
  let onboardingTryLimit = driverImagesInfo.transporterConfig.onboardingTryLimit
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  hvVerificationReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord idfyVerificationReq hvVerificationReq
  let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
  verificationStatusWithMessage onboardingTryLimit (length images) mbVerificationReqRecord language docType

verificationStatusWithMessage :: Int -> Int -> Maybe SDO.VerificationReqRecord -> Language -> DVC.DocumentType -> Flow (ResponseStatus, Text)
verificationStatusWithMessage onboardingTryLimit imagesNum mbVerificationReqRecord language docType =
  case mbVerificationReqRecord of
    Just req -> do
      mbRC <- case docType of
        DVC.VehicleRegistrationCertificate -> do
          registrationNoEither <- withTryCatch "decryptDocumentNumber:verificationStatusWithMessage" (decrypt req.documentNumber)
          case registrationNoEither of
            Left err -> do
              logError $ "Error while decrypting document number: " <> (req.documentNumber & unEncrypted . encrypted) <> " with err: " <> show err
              pure Nothing
            Right registrationNo -> do
              rcNoEnc <- encrypt registrationNo
              RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
        _ -> pure Nothing

      if req.status == "pending" || req.status == "source_down_retrying"
        then do
          msg <- toVerificationMessage VerificationInProgress language >>= addVerificationReasons language (mbRC <&> (.failedRules))
          return (PENDING, msg)
        else do
          message <- getMessageFromResponse language req.verificaitonResponse >>= addVerificationReasons language (mbRC <&> (.failedRules))
          return (FAILED, message)
    Nothing -> do
      if imagesNum > onboardingTryLimit * bool 1 2 (docType == DVC.DriverLicense)
        then do
          msg <- toVerificationMessage LimitExceed language
          return (LIMIT_EXCEED, msg)
        else do
          msg <- toVerificationMessage NoDcoumentFound language
          return (NO_DOC_AVAILABLE, msg)

getMessageFromResponse :: Language -> Maybe Text -> Flow Text
getMessageFromResponse language response = do
  case response of
    Just res
      | "id_not_found" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | "source_down" `T.isInfixOf` res -> toVerificationMessage VerificationInProgress language
      | "TIMEOUT" `T.isInfixOf` res -> toVerificationMessage VerficationFailed language
      | "BAD_REQUEST" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | "422" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | otherwise -> toVerificationMessage Other language
    Nothing -> toVerificationMessage Other language

data VerificationMessage
  = InvalidDocumentNumber
  | VerficationFailed
  | NoDcoumentFound
  | LimitExceed
  | DLInvalid
  | RCInvalid
  | DocumentInvalid
  | DocumentValid
  | VerificationInProgress
  | VerificationPendingOnUserInput
  | UnderManualReview
  | Unauthorized
  | PullRequired
  | ConsentDenied
  | Other
  | Reasons
  deriving (Show, Eq, Ord)

translateDynamicKey :: Text -> Language -> Flow Text
translateDynamicKey key lang = do
  mTranslation <- MTQuery.findByErrorAndLanguage key lang
  return $ fromMaybe key (mTranslation <&> (.message))

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"

getLatestVerificationRecord :: Maybe IV.IdfyVerification -> Maybe HV.HyperVergeVerification -> Maybe SDO.VerificationReqRecord
getLatestVerificationRecord mbIdfyVerificationReq mbHvVerificationReq = do
  case (mbIdfyVerificationReq <&> (.createdAt), mbHvVerificationReq <&> (.createdAt)) of
    (Just idfyCreatedAt, Just hvCreatedAt) -> if idfyCreatedAt > hvCreatedAt then SDO.makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq else SDO.makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Nothing, Just _) -> SDO.makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Just _, Nothing) -> SDO.makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq
    (Nothing, Nothing) -> Nothing

-- Convert CommonDriverOnboardingDocuments to CommonDocumentItem
mkCommonDocumentItem :: DCDOD.CommonDriverOnboardingDocuments -> CommonDocumentItem
mkCommonDocumentItem doc =
  CommonDocumentItem
    { documentType = doc.documentType,
      documentData = doc.documentData,
      verificationStatus = mapVerificationStatus doc.verificationStatus,
      rejectReason = doc.rejectReason,
      documentImageId = getId <$> doc.documentImageId,
      createdAt = doc.createdAt,
      updatedAt = doc.updatedAt
    }
  where
    mapVerificationStatus :: Documents.VerificationStatus -> ResponseStatus
    mapVerificationStatus Documents.PENDING = PENDING
    mapVerificationStatus Documents.VALID = VALID
    mapVerificationStatus Documents.INVALID = INVALID
    mapVerificationStatus Documents.MANUAL_VERIFICATION_REQUIRED = MANUAL_VERIFICATION_REQUIRED
    mapVerificationStatus _ = PENDING -- default case

getDigilockerResponseCode :: Id DP.Person -> Flow (Maybe Text)
getDigilockerResponseCode driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ mbSession >>= (.responseCode)

getDigilockerDocStatusMap :: Id DP.Person -> Flow DocStatus.DocStatusMap
getDigilockerDocStatusMap driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ maybe DocStatus.emptyDocStatusMap (.docStatus) mbSession

mapDigilockerToResponseStatus :: DocStatus.DocStatusEnum -> Maybe ResponseStatus
mapDigilockerToResponseStatus DocStatus.DOC_PENDING = Just PENDING
mapDigilockerToResponseStatus DocStatus.DOC_FAILED = Just FAILED
mapDigilockerToResponseStatus DocStatus.DOC_CONSENT_DENIED = Just CONSENT_DENIED
mapDigilockerToResponseStatus DocStatus.DOC_PULL_REQUIRED = Just PULL_REQUIRED
mapDigilockerToResponseStatus DocStatus.DOC_SUCCESS = Just VALID

checkInspectionHubRequestCreated :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DOHR.RequestType -> Maybe (Id DP.Person) -> Maybe Text -> m (Maybe DOHR.RequestStatus)
checkInspectionHubRequestCreated requestType mbDriverId mbRegistrationNo = do
  mbInspectionReq <- case requestType of
    DOHR.DRIVER_ONBOARDING_INSPECTION -> case mbDriverId of
      Just driverId -> runInReplica $ QOHRE.findLatestByDriverIdAndRequestType driverId requestType
      Nothing -> pure Nothing
    DOHR.ONBOARDING_INSPECTION -> case mbRegistrationNo of
      Just registrationNo -> runInReplica $ QOHRE.findLatestByRegistrationNoAndRequestType registrationNo requestType
      Nothing -> pure Nothing
    _ -> pure Nothing
  pure $ (.requestStatus) <$> mbInspectionReq

mapInspectionHubRequestStatusToResponseStatus :: Maybe DOHR.RequestStatus -> Maybe ResponseStatus
mapInspectionHubRequestStatusToResponseStatus mbRequestStatus = case mbRequestStatus of
  Just DOHR.APPROVED -> Just VALID
  Just DOHR.PENDING -> Just VALID
  Just DOHR.REJECTED -> Just INVALID
  Nothing -> Just INVALID

getInspectionHubStatusForResponseStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DOHR.RequestType -> Maybe (Id DP.Person) -> Maybe Text -> m (Maybe ResponseStatus)
getInspectionHubStatusForResponseStatus requestType mbDriverId mbRegistrationNo = do
  mbRequestStatus <- checkInspectionHubRequestCreated requestType mbDriverId mbRegistrationNo
  pure $ mapInspectionHubRequestStatusToResponseStatus mbRequestStatus
