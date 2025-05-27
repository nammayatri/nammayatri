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
    fetchDriverVehicleDocuments,
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

fetchDriverVehicleDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Maybe Bool ->
  Maybe Text ->
  Flow ([DocumentStatusItem], [VehicleDocumentItem])
fetchDriverVehicleDocuments personId merchantOperatingCity transporterConfig language useHVSdkForDL mbReqRegistrationNo = do
  driverImages <- IQuery.findAllByPersonId transporterConfig personId
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = personId, merchantOperatingCity, driverImages, transporterConfig, now}
  driverDocuments <- fetchDriverDocuments driverImagesInfo language useHVSdkForDL
  vehicleDocumentsUnverified <- fetchVehicleDocuments driverImagesInfo language mbReqRegistrationNo
  pure (driverDocuments, vehicleDocumentsUnverified)

statusHandler' ::
  IQuery.DriverImagesInfo ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe DL.DriverLicense ->
  Maybe Bool ->
  Flow StatusRes'
statusHandler' driverImagesInfo makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL useHVSdkForDL = do
  let merchantId = driverImagesInfo.merchantOperatingCity.merchantId
      merchantOperatingCity = driverImagesInfo.merchantOperatingCity
      merchantOpCityId = merchantOperatingCity.id
      transporterConfig = driverImagesInfo.transporterConfig
      personId = driverImagesInfo.driverId
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let language = fromMaybe merchantOperatingCity.language person.language

  driverDocuments <- fetchDriverDocuments driverImagesInfo language useHVSdkForDL
  let mbReqRegistrationNo = Nothing
  vehicleDocumentsUnverified <- fetchVehicleDocuments driverImagesInfo language mbReqRegistrationNo

  whenJust (onboardingVehicleCategory <|> (mDL >>= (.vehicleCategory))) $ \vehicleCategory -> do
    documentVerificationConfigs <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId vehicleCategory Nothing
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
        let vehRegImgIds = map (.id) $ IQuery.filterImagesByPersonAndType driverImagesInfo merchantId DVC.VehicleRegistrationCertificate
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
      let merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
          personId = driverImagesInfo.driverId
      vehicleDocumentsUnverified `forM` \vehicleDoc@VehicleDocumentItem {..} -> do
        allVehicleDocsVerified <- checkAllVehicleDocsVerified merchantOpCityId vehicleDoc makeSelfieAadhaarPanMandatory
        allDriverDocsVerified <- checkAllDriverDocsVerified merchantOpCityId driverDocuments vehicleDoc makeSelfieAadhaarPanMandatory
        when (allVehicleDocsVerified && allDriverDocsVerified && requiresOnboardingInspection /= Just True) $ enableDriver merchantOpCityId personId mDL

        mbVehicle <- QVehicle.findById personId -- check everytime
        when (isNothing mbVehicle && allVehicleDocsVerified && allDriverDocsVerified && isNothing multipleRC && requiresOnboardingInspection /= Just True) $
          void $ try @_ @SomeException (activateRCAutomatically personId driverImagesInfo.merchantOperatingCity vehicleDoc.registrationNo)
        if allVehicleDocsVerified then return VehicleDocumentItem {isVerified = True, ..} else return vehicleDoc

    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
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
            dateOfIssue = dl.dateOfIssue
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
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
            failedRules = rc.failedRules
          }

fetchDriverDocuments ::
  IQuery.DriverImagesInfo ->
  Language ->
  Maybe Bool ->
  Flow [DocumentStatusItem]
fetchDriverDocuments driverImagesInfo language useHVSdkForDL =
  driverDocumentTypes `forM` \docType -> do
    (mbStatus, mbProcessedReason, mbProcessedUrl) <- getProcessedDriverDocuments driverImagesInfo docType useHVSdkForDL
    case mbStatus of
      Just status -> do
        message <- documentStatusMessage status Nothing docType mbProcessedUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl}
      Nothing -> do
        (status, mbReason, mbUrl) <- getInProgressDriverDocuments driverImagesInfo docType
        message <- documentStatusMessage status mbReason docType mbUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}

fetchVehicleDocuments ::
  IQuery.DriverImagesInfo ->
  Language ->
  Maybe Text ->
  Flow [VehicleDocumentItem]
fetchVehicleDocuments driverImagesInfo language Nothing = do
  -- All items required
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC driverImagesInfo language Nothing
  processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo processedVehicleDocumentsWithRC Nothing
  let processedVehicleDocuments = processedVehicleDocumentsWithoutRC <> processedVehicleDocumentsWithRC
  inprogressVehicleDocuments <- fetchInprogressVehicleDocuments driverImagesInfo language processedVehicleDocuments Nothing
  pure $ processedVehicleDocuments <> inprogressVehicleDocuments
fetchVehicleDocuments driverImagesInfo language (Just reqRegistrationNo) = do
  -- Only one item required with specific registrationNo
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC driverImagesInfo language (Just reqRegistrationNo)
  let driverId = driverImagesInfo.driverId
  if null processedVehicleDocumentsWithRC
    then do
      processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo processedVehicleDocumentsWithRC (Just reqRegistrationNo)
      if null processedVehicleDocumentsWithoutRC
        then do
          docs <- fetchInprogressVehicleDocuments driverImagesInfo language [] (Just reqRegistrationNo)
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

fetchProcessedVehicleDocumentsWithRC ::
  IQuery.DriverImagesInfo ->
  Language ->
  Maybe Text ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithRC driverImagesInfo language mbReqRegistrationNo = do
  let personId = driverImagesInfo.driverId
  associations <- DRAQuery.findAllLinkedByDriverId personId
  processedVehicles <- (catMaybes <$>) $
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
  processedVehicles `forM` \(isActive, rcId, processedVehicle) -> do
    rcImages <- IQuery.findRecentByRcIdAndImageTypes driverImagesInfo.transporterConfig rcId vehicleDocsByRcIdList
    let rcImagesInfo = IQuery.RcImagesInfo {rcId, rcImages, documentTypes = vehicleDocsByRcIdList}
    registrationNo <- decrypt processedVehicle.certificateNumber
    let dateOfUpload = processedVehicle.createdAt
    documents <-
      vehicleDocumentTypes `forM` \docType -> do
        (mbStatus, mbProcessedReason, mbProcessedUrl) <- getProcessedVehicleDocuments driverImagesInfo docType processedVehicle
        case mbStatus of
          Just status -> do
            message <- documentStatusMessage status Nothing docType mbProcessedUrl language
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl}
          Nothing -> do
            (status, mbReason, mbUrl) <- getInProgressVehicleDocuments driverImagesInfo (Just rcImagesInfo) docType
            message <- documentStatusMessage status mbReason docType mbUrl language
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}
    return
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

fetchProcessedVehicleDocumentsWithoutRC ::
  IQuery.DriverImagesInfo ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithoutRC driverImagesInfo processedVehicleDocumentsWithRC mbReqRegistrationNo = do
  let personId = driverImagesInfo.driverId
  mbVehicle <- QVehicle.findById personId
  case mbVehicle of
    Just vehicle -> do
      let vehicleAlreadyIncluded = isJust $ find (\doc -> doc.registrationNo == vehicle.registrationNo) processedVehicleDocumentsWithRC
      -- filter by rcNo if required
      let wrongRcNo = isJust mbReqRegistrationNo && Just vehicle.registrationNo /= mbReqRegistrationNo
      if vehicleAlreadyIncluded || wrongRcNo
        then return []
        else do
          documents <-
            vehicleDocumentTypes `forM` \docType -> do
              return $ DocumentStatusItem {documentType = docType, verificationStatus = NO_DOC_AVAILABLE, verificationMessage = Nothing, verificationUrl = Nothing}
          return
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

fetchInprogressVehicleDocuments ::
  IQuery.DriverImagesInfo ->
  Language ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Flow [VehicleDocumentItem]
fetchInprogressVehicleDocuments driverImagesInfo language processedVehicleDocuments mbReqRegistrationNo = do
  let personId = driverImagesInfo.driverId
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
          -- filter by rcNo if required
          let wrongRcNo = isJust mbReqRegistrationNo && Just registrationNo /= mbReqRegistrationNo
          if wrongRcNo
            then return []
            else do
              rcNoEnc <- encrypt registrationNo
              rc <- RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
              (isUnlinked, mbRcId) <- case rc of
                Just rc_ -> do
                  iu <- DRAQuery.findUnlinkedRC personId rc_.id
                  pure (iu, Just rc_.id)
                Nothing -> pure ([], Nothing)
              mbRcImagesInfo <- forM mbRcId $ \rcId -> do
                rcImages <- IQuery.findRecentByRcIdAndImageTypes driverImagesInfo.transporterConfig rcId vehicleDocsByRcIdList
                pure IQuery.RcImagesInfo {rcId, rcImages, documentTypes = vehicleDocsByRcIdList}
              if isJust (find (\doc -> doc.registrationNo == registrationNo) processedVehicleDocuments) || not (null isUnlinked)
                then return []
                else do
                  documents <-
                    vehicleDocumentTypes `forM` \docType -> do
                      (status, mbReason, mbUrl) <- getInProgressVehicleDocuments driverImagesInfo mbRcImagesInfo docType
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
  mbVerificationConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId docType category Nothing
  case mbVerificationConfig of
    Just verificationConfig -> do
      if verificationConfig.isMandatory && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory)
        then case status of
          VALID -> return True
          MANUAL_VERIFICATION_REQUIRED -> return verificationConfig.isDefaultEnabledOnManualVerification
          _ -> return False
        else return True
    Nothing -> return True

getProcessedDriverDocuments :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Maybe Bool -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
getProcessedDriverDocuments driverImagesInfo docType useHVSdkForDL = do
  let driverId = driverImagesInfo.driverId
      merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
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
    DVC.ProfilePhoto -> return $ checkImageValidity driverImagesInfo DVC.ProfilePhoto
    DVC.UploadProfile -> return $ checkImageValidity driverImagesInfo DVC.UploadProfile
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

getProcessedVehicleDocuments :: IQuery.DriverImagesInfo -> DVC.DocumentType -> RC.VehicleRegistrationCertificate -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
getProcessedVehicleDocuments driverImagesInfo docType vehicleRC = do
  let driverId = driverImagesInfo.driverId
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
    DVC.VehicleInspectionForm -> return $ checkImageValidity driverImagesInfo DVC.VehicleInspectionForm
    DVC.SubscriptionPlan -> do
      mbPlan <- snd <$> DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION driverId -- fix later on basis of vehicle category
      return (Just $ boolToStatus (isJust mbPlan), Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing)
  where
    boolToStatus :: Bool -> ResponseStatus
    boolToStatus = \case
      True -> VALID
      False -> NO_DOC_AVAILABLE

checkImageValidity :: IQuery.DriverImagesInfo -> DVC.DocumentType -> (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
checkImageValidity driverImagesInfo docType = do
  let validImages = IQuery.filterImageByPersonIdAndImageTypeAndVerificationStatus driverImagesInfo docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == Just Documents.VALID) validImages = (Just VALID, Nothing, Nothing)
      | any (\img -> img.verificationStatus == Just Documents.MANUAL_VERIFICATION_REQUIRED) validImages = (Just MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
      | otherwise = (Nothing, Nothing, Nothing)

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
  IQuery.DriverImagesInfo ->
  DDVC.DocumentType ->
  Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
getInProgressDriverDocuments driverImagesInfo docType = do
  let driverId = driverImagesInfo.driverId
      merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
      merchantId = driverImagesInfo.merchantOperatingCity.merchantId
  case docType of
    DDVC.DriverLicense -> checkIfUnderProgress driverImagesInfo DDVC.DriverLicense
    DDVC.BackgroundVerification -> checkBackgroundVerificationStatus driverId merchantId merchantOpCityId
    DDVC.AadhaarCard -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.AadhaarCard
    DDVC.PanCard -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.PanCard
    DDVC.GSTCertificate -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.GSTCertificate
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      let mbImages = IQuery.filterRecentLatestByPersonIdAndImageType driverImagesInfo DDVC.ProfilePhoto
      return (fromMaybe NO_DOC_AVAILABLE (mapStatus <$> (mbImages >>= (.verificationStatus))), Nothing, Nothing)
    DDVC.UploadProfile -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DDVC.UploadProfile
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

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

getInProgressVehicleDocuments :: IQuery.DriverImagesInfo -> Maybe IQuery.RcImagesInfo -> DVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
getInProgressVehicleDocuments driverImagesInfo mbRcImagesInfo docType =
  case docType of
    DVC.VehicleRegistrationCertificate -> checkIfUnderProgress driverImagesInfo DVC.VehicleRegistrationCertificate
    DVC.SubscriptionPlan -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    DVC.VehiclePermit -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehiclePermit
    DVC.VehicleFitnessCertificate -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleFitnessCertificate
    DVC.VehicleInsurance -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleInsurance
    DVC.VehiclePUC -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehiclePUC
    DVC.VehicleInspectionForm -> return $ checkIfImageUploadedOrInvalidated driverImagesInfo DVC.VehicleInspectionForm
    _ | docType `elem` vehicleDocsByRcIdList -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo docType
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

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

checkIfImageUploadedOrInvalidated :: IQuery.DriverImagesInfo -> DDVC.DocumentType -> (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated driverImagesInfo docType = do
  let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
  case images of
    [] -> (NO_DOC_AVAILABLE, Nothing, Nothing)
    latestImage : _ -> do
      if latestImage.verificationStatus == Just Documents.INVALID
        then (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        else (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

checkIfUnderProgress :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfUnderProgress driverImagesInfo docType = do
  let driverId = driverImagesInfo.driverId
  mbVerificationReqIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  mbVerificationReqHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord mbVerificationReqIdfy mbVerificationReqHV
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

getDLAndStatus :: IQuery.DriverImagesInfo -> Language -> Maybe Bool -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverImagesInfo language useHVSdkForDL = do
  let driverId = driverImagesInfo.driverId
      merchantOpCityId = driverImagesInfo.merchantOperatingCity.id
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
        (status, message) <- checkIfInVerification driverImagesInfo DVC.DriverLicense language
        return (status, message)
  return (status, mDriverLicense, message)

getRCAndStatus :: IQuery.DriverImagesInfo -> Maybe Bool -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
getRCAndStatus driverImagesInfo multipleRC language = do
  let driverId = driverImagesInfo.driverId
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      (status, message) <- checkIfInVerification driverImagesInfo DVC.VehicleRegistrationCertificate language
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

checkIfInVerification :: IQuery.DriverImagesInfo -> DVC.DocumentType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverImagesInfo docType language = do
  let driverId = driverImagesInfo.driverId
      onboardingTryLimit = driverImagesInfo.transporterConfig.onboardingTryLimit
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  hvVerificationReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord idfyVerificationReq hvVerificationReq
  let images = IQuery.filterRecentByPersonIdAndImageType driverImagesInfo docType
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
