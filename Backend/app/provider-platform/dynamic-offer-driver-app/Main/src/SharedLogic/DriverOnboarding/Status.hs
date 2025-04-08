module SharedLogic.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes' (..),
    VehicleDocumentItem (..),
    DocumentStatusItem (..),
    DLDetails (..),
    RCDetails (..),
    statusHandler',
    getDLAndStatus,
    getRCAndStatus,
    getAadhaarStatus,
    mapStatus,
    fetchDriverDocuments,
    fetchVehicleDocuments,
    checkAllVehicleDocsVerified,
    checkAllDriverDocsVerified,
    activateRCAutomatically,
  )
where

import Control.Applicative ((<|>))
import qualified Control.Monad.Extra as Extra
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Plan as DAPlan
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverLicense as DL
import qualified Domain.Types.HyperVergeVerification as HV
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan as Plan
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as BVQuery
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
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleFitnessCertificate as VFCQuery
import qualified Storage.Queries.VehicleInsurance as VIQuery
import qualified Storage.Queries.VehiclePUC as VPUCQuery
import qualified Storage.Queries.VehiclePermit as VPQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.BackgroundVerification as BackgroundVerification
import Tools.Error (DriverOnboardingError (ImageNotValid))
import qualified Tools.Verification as Verification

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- UNAUTHORIZED is used when a driver is not eligible to be onboarded to the platform
-- INVALID is the state
--   which the doc switches to when, for example, it's expired or when it is invalidated from dashboard.
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED | UNAUTHORIZED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes' = StatusRes'
  { driverDocuments :: [DocumentStatusItem],
    vehicleDocuments :: [VehicleDocumentItem],
    enabled :: Bool,
    manualVerificationRequired :: Maybe Bool,
    driverLicenseDetails :: Maybe [DLDetails],
    vehicleRegistrationCertificateDetails :: Maybe [RCDetails]
  }

data VehicleDocumentItem = VehicleDocumentItem
  { registrationNo :: Text,
    userSelectedVehicleCategory :: DVC.VehicleCategory,
    verifiedVehicleCategory :: Maybe DVC.VehicleCategory,
    isVerified :: Bool,
    isActive :: Bool,
    vehicleModel :: Maybe Text,
    documents :: [DocumentStatusItem],
    dateOfUpload :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: DDVC.DocumentType,
    verificationStatus :: ResponseStatus,
    verificationMessage :: Maybe Text,
    verificationUrl :: Maybe BaseUrl
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
    createdAt :: UTCTime
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
    failedRules :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

statusHandler' ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe DL.DriverLicense ->
  Maybe Bool ->
  Flow StatusRes'
statusHandler' personId merchantOperatingCity transporterConfig makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL useHVSdkForDL = do
  let merchantId = merchantOperatingCity.merchantId
      merchantOpCityId = merchantOperatingCity.id
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let language = fromMaybe merchantOperatingCity.language person.language

  driverDocuments <- fetchDriverDocuments personId merchantOperatingCity transporterConfig language useHVSdkForDL

  vehicleDocumentsUnverified <- fetchVehicleDocuments personId merchantOperatingCity transporterConfig language

  whenJust (onboardingVehicleCategory <|> (mDL >>= (.vehicleCategory))) $ \vehicleCategory -> do
    documentVerificationConfigs <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId vehicleCategory
    let mandatoryVehicleDocumentVerificationConfigs = filter (\config -> config.documentType `elem` vehicleDocumentTypes && config.isMandatory) documentVerificationConfigs
    when (null mandatoryVehicleDocumentVerificationConfigs) $ do
      allDriverDocsVerified <- Extra.allM (\doc -> checkIfDocumentValid merchantOpCityId doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments
      when (allDriverDocsVerified && transporterConfig.requiresOnboardingInspection /= Just True && person.role == DP.DRIVER) $ do
        enableDriver merchantOpCityId personId mDL
        whenJust onboardingVehicleCategory $ \category -> do
          DIIQuery.updateOnboardingVehicleCategory (Just category) personId

  -- check if driver is enabled if not then if all mandatory docs are verified then enable the driver
  vehicleDocuments <- getVehicleDocuments driverDocuments vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection

  (dlDetails, rcDetails) <-
    case prefillData of
      Just True -> do
        vehRegImgIds <- map (.id) <$> runInReplica (IQuery.findImagesByPersonAndType merchantId personId DVC.VehicleRegistrationCertificate)
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

  return $
    StatusRes'
      { driverDocuments,
        vehicleDocuments,
        enabled = enabled,
        manualVerificationRequired = transporterConfig.requiresOnboardingInspection,
        driverLicenseDetails = dlDetails,
        vehicleRegistrationCertificateDetails = rcDetails
      }
  where
    getVehicleDocuments driverDocuments vehicleDocumentsUnverified requiresOnboardingInspection = do
      let merchantOpCityId = merchantOperatingCity.id
      vehicleDocumentsUnverified `forM` \vehicleDoc@VehicleDocumentItem {..} -> do
        allVehicleDocsVerified <- checkAllVehicleDocsVerified merchantOpCityId vehicleDoc makeSelfieAadhaarPanMandatory
        allDriverDocsVerified <- checkAllDriverDocsVerified merchantOpCityId driverDocuments vehicleDoc makeSelfieAadhaarPanMandatory
        when (allVehicleDocsVerified && allDriverDocsVerified && requiresOnboardingInspection /= Just True) $ enableDriver merchantOpCityId personId mDL

        mbVehicle <- QVehicle.findById personId -- check everytime
        when (isNothing mbVehicle && allVehicleDocsVerified && allDriverDocsVerified && isNothing multipleRC && requiresOnboardingInspection /= Just True) $
          void $ try @_ @SomeException (activateRCAutomatically personId merchantOperatingCity vehicleDoc.registrationNo)
        if allVehicleDocsVerified then return VehicleDocumentItem {isVerified = True, ..} else return vehicleDoc

    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      pure $
        DLDetails
          { driverName = dl.driverName,
            driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show merchantOperatingCity.city,
            driverDateOfBirth = dl.driverDob,
            classOfVehicles = dl.classOfVehicles,
            imageId1 = dl.documentImageId1.getId,
            imageId2 = getId <$> dl.documentImageId2,
            createdAt = dl.createdAt,
            dateOfIssue = dl.dateOfIssue
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
      pure $
        RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show merchantOperatingCity.city,
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
            failedRules = rc.failedRules
          }

fetchDriverDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Maybe Bool ->
  Flow [DocumentStatusItem]
fetchDriverDocuments personId merchantOpCity transporterConfig language useHVSdkForDL = do
  let merchantId = merchantOpCity.merchantId
      merchantOpCityId = merchantOpCity.id
  driverDocumentTypes `forM` \docType -> do
    (mbStatus, mbProcessedReason, mbProcessedUrl) <- getProcessedDriverDocuments docType personId merchantId merchantOpCityId useHVSdkForDL
    case mbStatus of
      Just status -> do
        message <- documentStatusMessage status Nothing docType mbProcessedUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl}
      Nothing -> do
        (status, mbReason, mbUrl) <- getInProgressDriverDocuments docType personId transporterConfig.onboardingTryLimit merchantId merchantOpCityId
        message <- documentStatusMessage status mbReason docType mbUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}

fetchVehicleDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow [VehicleDocumentItem]
fetchVehicleDocuments personId merchantOpCity transporterConfig language = do
  processedVehicleDocuments <- fetchProcessedVehicleDocuments personId merchantOpCity transporterConfig language
  inprogressVehicleDocuments <- fetchInprogressVehicleDocuments personId merchantOpCity transporterConfig language processedVehicleDocuments
  pure $ processedVehicleDocuments <> inprogressVehicleDocuments

fetchProcessedVehicleDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocuments personId merchantOpCity transporterConfig language = do
  let merchantId = merchantOpCity.merchantId
      merchantOpCityId = merchantOpCity.id
  processedVehicleDocumentsWithRC <- do
    processedVehicles <- do
      associations <- DRAQuery.findAllLinkedByDriverId personId
      if null associations
        then return []
        else (associations `forM` (\assoc -> RCQuery.findById assoc.rcId >>= (\rc -> return $ (assoc.isRcActive,) <$> rc))) >>= (return . catMaybes)
    processedVehicles `forM` \(isActive, processedVehicle) -> do
      registrationNo <- decrypt processedVehicle.certificateNumber
      let dateOfUpload = processedVehicle.createdAt
      documents <-
        vehicleDocumentTypes `forM` \docType -> do
          (mbStatus, mbProcessedReason, mbProcessedUrl) <- getProcessedVehicleDocuments docType personId processedVehicle merchantId merchantOpCityId
          case mbStatus of
            Just status -> do
              message <- documentStatusMessage status Nothing docType mbProcessedUrl language
              return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl}
            Nothing -> do
              (status, mbReason, mbUrl) <- getInProgressVehicleDocuments docType personId transporterConfig.onboardingTryLimit merchantId merchantOpCityId
              message <- documentStatusMessage status mbReason docType mbUrl language
              return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}
      return $
        VehicleDocumentItem
          { registrationNo,
            userSelectedVehicleCategory = fromMaybe (maybe DVC.CAR DV.castVehicleVariantToVehicleCategory processedVehicle.vehicleVariant) processedVehicle.userPassedVehicleCategory,
            verifiedVehicleCategory = DV.castVehicleVariantToVehicleCategory <$> processedVehicle.vehicleVariant,
            isVerified = False,
            isActive,
            vehicleModel = processedVehicle.vehicleModel,
            documents,
            dateOfUpload
          }

  processedVehicleDocumentsWithoutRC <- do
    mbVehicle <- QVehicle.findById personId
    case mbVehicle of
      Just vehicle -> do
        if isJust $ find (\doc -> doc.registrationNo == vehicle.registrationNo) processedVehicleDocumentsWithRC
          then return []
          else do
            documents <-
              vehicleDocumentTypes `forM` \docType -> do
                return $ DocumentStatusItem {documentType = docType, verificationStatus = NO_DOC_AVAILABLE, verificationMessage = Nothing, verificationUrl = Nothing}
            return $
              [ VehicleDocumentItem
                  { registrationNo = vehicle.registrationNo,
                    userSelectedVehicleCategory = DV.castVehicleVariantToVehicleCategory vehicle.variant,
                    verifiedVehicleCategory = Just $ DV.castVehicleVariantToVehicleCategory vehicle.variant,
                    isVerified = True,
                    isActive = True,
                    vehicleModel = Just vehicle.model,
                    documents,
                    dateOfUpload = vehicle.createdAt
                  }
              ]
      Nothing -> return []

  pure $ processedVehicleDocumentsWithoutRC <> processedVehicleDocumentsWithRC

fetchInprogressVehicleDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  [VehicleDocumentItem] ->
  Flow [VehicleDocumentItem]
fetchInprogressVehicleDocuments personId merchantOpCity transporterConfig language processedVehicleDocuments = do
  let merchantId = merchantOpCity.merchantId
      merchantOpCityId = merchantOpCity.id
  inprogressVehicleIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing personId DVC.VehicleRegistrationCertificate
  inprogressVehicleHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing personId DVC.VehicleRegistrationCertificate
  let mbVerificationReqRecord = getLatestVerificationRecord inprogressVehicleIdfy inprogressVehicleHV
  case mbVerificationReqRecord of
    Just verificationReqRecord -> do
      registrationNoEither <- try @_ @SomeException (decrypt verificationReqRecord.documentNumber)
      case registrationNoEither of
        Left err -> do
          logError $ "Error while decrypting document number: " <> (verificationReqRecord.documentNumber & unEncrypted . encrypted) <> " with err: " <> show err
          return []
        Right registrationNo -> do
          rcNoEnc <- encrypt registrationNo
          rc <- RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
          isUnlinked <- case rc of
            Just rc_ -> DRAQuery.findUnlinkedRC personId rc_.id
            Nothing -> pure []
          if isJust (find (\doc -> doc.registrationNo == registrationNo) processedVehicleDocuments) || not (null isUnlinked)
            then return []
            else do
              documents <-
                vehicleDocumentTypes `forM` \docType -> do
                  (status, mbReason, mbUrl) <- getInProgressVehicleDocuments docType personId transporterConfig.onboardingTryLimit merchantId merchantOpCityId
                  message <- documentStatusMessage status mbReason docType mbUrl language
                  return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}
              return
                [ VehicleDocumentItem
                    { registrationNo,
                      userSelectedVehicleCategory = fromMaybe DVC.CAR verificationReqRecord.vehicleCategory,
                      verifiedVehicleCategory = Nothing,
                      isVerified = False,
                      isActive = False,
                      vehicleModel = Nothing,
                      documents,
                      dateOfUpload = verificationReqRecord.createdAt
                    }
                ]
    Nothing -> return []

checkAllVehicleDocsVerified ::
  Id DMOC.MerchantOperatingCity ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Flow Bool
checkAllVehicleDocsVerified merchantOpCityId vehicleDoc makeSelfieAadhaarPanMandatory = do
  Extra.allM (\doc -> checkIfDocumentValid merchantOpCityId doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus makeSelfieAadhaarPanMandatory) vehicleDoc.documents

checkAllDriverDocsVerified ::
  Id DMOC.MerchantOperatingCity ->
  [DocumentStatusItem] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Flow Bool
checkAllDriverDocsVerified merchantOpCityId driverDocuments vehicleDoc makeSelfieAadhaarPanMandatory = do
  Extra.allM (\doc -> checkIfDocumentValid merchantOpCityId doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments

enableDriver :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing = return ()
enableDriver merchantOpCityId personId (Just dl) = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  unless driverInfo.enabled $ do
    enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId True
    whenJust dl.driverName $ \name -> QPerson.updateName name personId

activateRCAutomatically :: Id DP.Person -> DMOC.MerchantOperatingCity -> Text -> Flow ()
activateRCAutomatically personId merchantOpCity rcNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantOpCity.merchantId, merchantOpCity.id) rcStatusReq

checkIfDocumentValid ::
  Id DMOC.MerchantOperatingCity ->
  DDVC.DocumentType ->
  DVC.VehicleCategory ->
  ResponseStatus ->
  Maybe Bool ->
  Flow Bool
checkIfDocumentValid merchantOpCityId docType category status makeSelfieAadhaarPanMandatory = do
  mbVerificationConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId docType category
  case mbVerificationConfig of
    Just verificationConfig -> do
      if verificationConfig.isMandatory && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory)
        then case status of
          VALID -> return True
          MANUAL_VERIFICATION_REQUIRED -> return verificationConfig.isDefaultEnabledOnManualVerification
          _ -> return False
        else return True
    Nothing -> return True

getProcessedDriverDocuments :: DVC.DocumentType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Bool -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
getProcessedDriverDocuments docType driverId _merchantId merchantOpCityId useHVSdkForDL =
  case docType of
    DVC.DriverLicense -> do
      mbDL <- DLQuery.findByDriverId driverId -- add failure reason in dl and rc
      if isNothing mbDL && (useHVSdkForDL == Just True)
        then do
          void $ try @_ @SomeException $ callGetDLGetStatus driverId merchantOpCityId
          mbDL' <- DLQuery.findByDriverId driverId
          return (mapStatus <$> (mbDL' <&> (.verificationStatus)), mbDL' >>= (.rejectReason), Nothing)
        else return (mapStatus <$> (mbDL <&> (.verificationStatus)), mbDL >>= (.rejectReason), Nothing)
    DVC.AadhaarCard -> do
      mbAadhaarCard <- QAadhaarCard.findByPrimaryKey driverId
      return (mapStatus . (.verificationStatus) <$> mbAadhaarCard, Nothing, Nothing)
    DVC.Permissions -> return (Just VALID, Nothing, Nothing)
    DVC.SocialSecurityNumber -> do
      mbSSN <- QDSSN.findByDriverId driverId
      return (mapStatus <$> (mbSSN <&> (.verificationStatus)), mbSSN >>= (.rejectReason), Nothing)
    DVC.ProfilePhoto -> checkImageValidity DVC.ProfilePhoto driverId
    DVC.UploadProfile -> checkImageValidity DVC.UploadProfile driverId
    DVC.PanCard -> do
      mbPanCard <- QDPC.findByDriverId driverId
      return (mapStatus . (.verificationStatus) <$> mbPanCard, Nothing, Nothing)
    DVC.GSTCertificate -> do
      mbGSTCertificate <- QDGST.findByDriverId driverId
      return (mapStatus . (.verificationStatus) <$> mbGSTCertificate, Nothing, Nothing)
    DVC.BackgroundVerification -> do
      mbBackgroundVerification <- BVQuery.findByDriverId driverId
      if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
        then return (Just VALID, Nothing, Nothing)
        else return (Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing)

callGetDLGetStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Flow ()
callGetDLGetStatus driverId merchantOpCityId = do
  latestReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId DVC.DriverLicense
  whenJust latestReq $ \verificationReq -> do
    when (verificationReq.status == "pending" || verificationReq.status == "source_down_retrying") $ do
      rsp <- Verification.getTask merchantOpCityId KEV.HyperVergeRCDL (KEV.GetTaskReq (Just "checkDL") verificationReq.requestId) HVQuery.updateResponse
      case rsp of
        KEV.DLResp resp -> do
          logDebug $ "callGetDLGetStatus: getTask api response for request id : " <> verificationReq.requestId <> " is : " <> show resp
          unless ("still being processed" `T.isInfixOf` (fromMaybe "" resp.message)) (void $ DDL.onVerifyDL (makeHVVerificationReqRecord verificationReq) resp KEV.HyperVergeRCDL)
        _ -> throwError $ InternalError "Document and apiEndpoint mismatch occurred !!!!!!!!"

getProcessedVehicleDocuments :: DVC.DocumentType -> Id DP.Person -> RC.VehicleRegistrationCertificate -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
getProcessedVehicleDocuments docType driverId vehicleRC _merchantId _merchantOpCityId =
  case docType of
    DVC.VehicleRegistrationCertificate -> return (Just $ mapStatus vehicleRC.verificationStatus, vehicleRC.rejectReason, Nothing)
    DVC.VehiclePermit -> do
      mbDoc <- listToMaybe <$> VPQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing)
    DVC.VehicleFitnessCertificate -> do
      mbDoc <- listToMaybe <$> VFCQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing)
    DVC.VehicleInsurance -> do
      mbDoc <- listToMaybe <$> VIQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), (mbDoc >>= (.rejectReason)), Nothing)
    DVC.VehiclePUC -> do
      mbDoc <- listToMaybe <$> VPUCQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing)
    DVC.VehicleInspectionForm -> checkImageValidity DVC.VehicleInspectionForm driverId
    DVC.SubscriptionPlan -> do
      mbPlan <- snd <$> DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION driverId -- fix later on basis of vehicle category
      return (Just $ boolToStatus (isJust mbPlan), Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing)
  where
    boolToStatus :: Bool -> ResponseStatus
    boolToStatus = \case
      True -> VALID
      False -> NO_DOC_AVAILABLE

checkImageValidity :: DVC.DocumentType -> Id DP.Person -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
checkImageValidity docType driverId = do
  validImages <- IQuery.findImageByPersonIdAndImageTypeAndVerificationStatus driverId docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == (Just Documents.VALID)) validImages = return (Just VALID, Nothing, Nothing)
      | any (\img -> img.verificationStatus == (Just Documents.MANUAL_VERIFICATION_REQUIRED)) validImages = return (Just MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
      | otherwise = return (Nothing, Nothing, Nothing)

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
  DDVC.DocumentType ->
  Id DP.Person ->
  Int ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
getInProgressDriverDocuments docType driverId onboardingTryLimit merchantId merchantOpCityId =
  case docType of
    DDVC.DriverLicense -> checkIfUnderProgress DDVC.DriverLicense driverId onboardingTryLimit
    DDVC.BackgroundVerification -> checkBackgroundVerificationStatus driverId merchantId merchantOpCityId
    DDVC.AadhaarCard -> checkIfImageUploadedOrInvalidated DDVC.AadhaarCard driverId
    DDVC.PanCard -> checkIfImageUploadedOrInvalidated DDVC.PanCard driverId
    DDVC.GSTCertificate -> checkIfImageUploadedOrInvalidated DDVC.GSTCertificate driverId
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      mbImages <- IQuery.findRecentLatestByPersonIdAndImageType driverId DDVC.ProfilePhoto
      return (fromMaybe NO_DOC_AVAILABLE (mapStatus <$> (mbImages >>= (.verificationStatus))), Nothing, Nothing)
    DDVC.UploadProfile -> checkIfImageUploadedOrInvalidated DDVC.UploadProfile driverId
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

getInProgressVehicleDocuments :: DVC.DocumentType -> Id DP.Person -> Int -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
getInProgressVehicleDocuments docType driverId onboardingTryLimit _merchantId _merchantOpCityId =
  case docType of
    DVC.VehicleRegistrationCertificate -> checkIfUnderProgress DVC.VehicleRegistrationCertificate driverId onboardingTryLimit
    DVC.SubscriptionPlan -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    DVC.VehiclePermit -> checkIfImageUploadedOrInvalidated DVC.VehiclePermit driverId
    DVC.VehicleFitnessCertificate -> checkIfImageUploadedOrInvalidated DVC.VehicleFitnessCertificate driverId
    DVC.VehicleInsurance -> checkIfImageUploadedOrInvalidated DVC.VehicleInsurance driverId
    DVC.VehiclePUC -> checkIfImageUploadedOrInvalidated DVC.VehiclePUC driverId
    DVC.VehicleInspectionForm -> checkIfImageUploadedOrInvalidated DVC.VehicleInspectionForm driverId
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

checkIfImageUploadedOrInvalidated :: DDVC.DocumentType -> Id DP.Person -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated docType driverId = do
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
  case images of
    [] -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    _ -> do
      let latestImage = head images
      if latestImage.verificationStatus == Just Documents.INVALID
        then return (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        else return (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

checkIfUnderProgress :: DVC.DocumentType -> Id DP.Person -> Int -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfUnderProgress docType driverId onboardingTryLimit = do
  mbVerificationReqIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  mbVerificationReqHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord mbVerificationReqIdfy mbVerificationReqHV
  case mbVerificationReqRecord of
    Just verificationReqRecord -> do
      if verificationReqRecord.status == "pending" || verificationReqRecord.status == "source_down_retrying"
        then return (PENDING, Nothing, Nothing)
        else return (FAILED, verificationReqRecord.verificaitonResponse, Nothing)
    Nothing -> do
      images <- IQuery.findRecentByPersonIdAndImageType driverId docType
      handleImages images
  where
    handleImages images
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

getDLAndStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Language -> Maybe Bool -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverId merchantOpCityId onboardingTryLimit language useHVSdkForDL = do
  mDriverLicense <- do
    mbDL' <- DLQuery.findByDriverId driverId
    case mbDL' of
      Just dl -> return $ Just dl
      Nothing -> do
        if useHVSdkForDL == Just True
          then do
            void $ try @_ @SomeException $ callGetDLGetStatus driverId merchantOpCityId
            DLQuery.findByDriverId driverId
          else return Nothing
  (status, message) <-
    case mDriverLicense of
      Just driverLicense -> do
        let status = mapStatus driverLicense.verificationStatus
        msg <- verificationStatusCheck status language DVC.DriverLicense
        return (status, msg)
      Nothing -> do
        (status, message) <- checkIfInVerification driverId merchantOpCityId onboardingTryLimit DVC.DriverLicense language
        return (status, message)
  return (status, mDriverLicense, message)

getRCAndStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Maybe Bool -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
getRCAndStatus driverId merchantOpCityId onboardingTryLimit multipleRC language = do
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      (status, message) <- checkIfInVerification driverId merchantOpCityId onboardingTryLimit DVC.VehicleRegistrationCertificate language
      return (status, Nothing, message)
    else do
      mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
      let vehicleRCs = catMaybes mVehicleRCs
      if isNothing multipleRC -- for backward compatibility
        then do
          let firstRC = listToMaybe vehicleRCs
          case firstRC of
            Just vehicleRC -> do
              let status = mapStatus vehicleRC.verificationStatus
              message <- verificationStatusCheck status language DVC.VehicleRegistrationCertificate
              return (status, Just vehicleRC, message)
            Nothing -> do
              msg <- toVerificationMessage NoDcoumentFound language
              return (NO_DOC_AVAILABLE, Nothing, msg)
        else do
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
                  message <- verificationStatusCheck status language DVC.VehicleRegistrationCertificate
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

verificationStatusCheck :: ResponseStatus -> Language -> DVC.DocumentType -> Flow Text
verificationStatusCheck status language img = do
  case (status, img) of
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> toVerificationMessage RCInvalid language
    _ -> toVerificationMessage DocumentValid language

checkIfInVerification :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Int -> DVC.DocumentType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverId _merchantOpCityId onboardingTryLimit docType language = do
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  hvVerificationReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord idfyVerificationReq hvVerificationReq
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
  verificationStatusWithMessage onboardingTryLimit (length images) mbVerificationReqRecord language docType

verificationStatusWithMessage :: Int -> Int -> Maybe VerificationReqRecord -> Language -> DVC.DocumentType -> Flow (ResponseStatus, Text)
verificationStatusWithMessage onboardingTryLimit imagesNum mbVerificationReqRecord language docType =
  case mbVerificationReqRecord of
    Just req -> do
      if req.status == "pending" || req.status == "source_down_retrying"
        then do
          msg <- toVerificationMessage VerificationInProgress language
          return (PENDING, msg)
        else do
          message <- getMessageFromResponse language req.verificaitonResponse
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
  | Other
  deriving (Show, Eq, Ord)

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"

getLatestVerificationRecord :: Maybe IV.IdfyVerification -> Maybe HV.HyperVergeVerification -> Maybe VerificationReqRecord
getLatestVerificationRecord mbIdfyVerificationReq mbHvVerificationReq = do
  case (mbIdfyVerificationReq <&> (.createdAt), mbHvVerificationReq <&> (.createdAt)) of
    (Just idfyCreatedAt, Just hvCreatedAt) -> if idfyCreatedAt > hvCreatedAt then makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq else makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Nothing, Just _) -> makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Just _, Nothing) -> makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq
    (Nothing, Nothing) -> Nothing
