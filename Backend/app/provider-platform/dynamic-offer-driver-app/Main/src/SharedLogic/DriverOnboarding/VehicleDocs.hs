{-# LANGUAGE OverloadedLabels #-}

module SharedLogic.DriverOnboarding.VehicleDocs where

import Control.Applicative ((<|>))
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Domain.Types.HyperVergeVerification as HV
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperationHubRequests as DOHR
import qualified Domain.Types.Person as DP
import Domain.Types.Plan as Plan
import qualified Domain.Types.ReviewRequest as DRR
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.OperationHubRequestsExtra as QOHRE
import qualified Storage.Queries.ReviewRequestExtra as SQRR
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleFitnessCertificate as VFCQuery
import qualified Storage.Queries.VehicleInsurance as VIQuery
import qualified Storage.Queries.VehicleNOC as VNOCQuery
import qualified Storage.Queries.VehiclePUC as VPUCQuery
import qualified Storage.Queries.VehiclePermit as VPQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error (DriverOnboardingError (FaceMatchFailed, ImageNotValid))

type DocVerificationConfigs = Either [FODVC.FleetOwnerDocumentVerificationConfig] [DVC.DocumentVerificationConfig]

data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED | UNAUTHORIZED | PULL_REQUIRED | CONSENT_DENIED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

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
    imageId :: Maybe Text,
    documentExpiry :: Maybe UTCTime,
    docsVerificationStatus :: Maybe DDVS.DocsVerificationStatus
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DLDocumentMetadata = DLDocumentMetadata
  { driverLicenseNumber :: Text,
    driverDateOfBirth :: Maybe UTCTime,
    dateOfExpiry :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AadhaarDocumentMetadata = AadhaarDocumentMetadata
  { aadhaarNumber :: Maybe Text,
    nameOnCard :: Maybe Text,
    dateOfBirth :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PanDocumentMetadata = PanDocumentMetadata
  { panNumber :: Text,
    panDocType :: Maybe DPan.PanType,
    driverDob :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data LocalAddressProofDocumentMetadata = LocalAddressProofDocumentMetadata
  { state :: Maybe Context.IndianState,
    proofDocumentType :: Maybe DI.AddressDocumentType,
    address :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype GSTDocumentMetadata = GSTDocumentMetadata
  { gstNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RCDocumentMetadata = RCDocumentMetadata
  { fitnessExpiry :: UTCTime,
    vehicleNumberPlate :: Text,
    vehicleVariant :: Maybe Text,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleModelYear :: Maybe Int,
    vehicleColor :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data VehiclePUCDocumentMetadata = VehiclePUCDocumentMetadata
  { pucNumber :: Maybe Text,
    pucExpiry :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data VehicleFitnessCertificateDocumentMetadata = VehicleFitnessCertificateDocumentMetadata
  { fitnessExpiry :: UTCTime,
    applicationNumber :: Text,
    rcNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data VehicleInsuranceDocumentMetadata = VehicleInsuranceDocumentMetadata
  { policyNumber :: Text,
    insuranceExpiry :: UTCTime,
    insuranceProvider :: Text,
    rcNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data VehiclePermitDocumentMetadata = VehiclePermitDocumentMetadata
  { permitNumber :: Text,
    permitExpiry :: UTCTime,
    regionCovered :: Text,
    rcNumber :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UDYAMDocumentMetadata = UDYAMDocumentMetadata
  { udyamNumber :: Maybe Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TANDocumentMetadata = TANDocumentMetadata
  { documentId :: Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data LDCDocumentMetadata = LDCDocumentMetadata
  { documentId :: Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DocumentMetadata
  = DLMetadata DLDocumentMetadata
  | AadhaarMetadata AadhaarDocumentMetadata
  | PanMetadata PanDocumentMetadata
  | LocalAddressProofMetadata LocalAddressProofDocumentMetadata
  | GSTMetadata GSTDocumentMetadata
  | RCMetadata RCDocumentMetadata
  | VehiclePUCMetadata VehiclePUCDocumentMetadata
  | VehicleFitnessMetadata VehicleFitnessCertificateDocumentMetadata
  | VehicleInsuranceMetadata VehicleInsuranceDocumentMetadata
  | VehiclePermitMetadata VehiclePermitDocumentMetadata
  | UDYAMMetadata UDYAMDocumentMetadata
  | TANMetadata TANDocumentMetadata
  | LDCMetadata LDCDocumentMetadata
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: DDVC.DocumentType,
    verificationStatus :: ResponseStatus,
    verificationMessage :: Maybe Text,
    verificationUrl :: Maybe BaseUrl,
    s3Path :: Maybe Text,
    imageId :: Maybe Text,
    imageId2 :: Maybe Text,
    documentExpiry :: Maybe UTCTime,
    metadata :: Maybe DocumentMetadata
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

validateMandatoryVehicleDocsForRC ::
  DTC.TransporterConfig ->
  RC.VehicleRegistrationCertificate ->
  Flow ()
validateMandatoryVehicleDocsForRC transporterConfig rc =
  when (transporterConfig.enableManualDocumentStatusCheck == Just True) $ do
    merchantOpCityId <- rc.merchantOperatingCityId & fromMaybeM (InternalError $ "merchantOperatingCityId missing for RC " <> rc.id.getId)
    merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
    registrationNo <- decrypt rc.certificateNumber
    let entity = IQuery.VehicleRCEntity rc
    entityImages <- IQuery.findAllByEntityId transporterConfig entity
    now <- getCurrentTime
    let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
        language = merchantOperatingCity.language
        onlyMandatoryDocs = Just True
        skipMessages = True
    allDocumentVerificationConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing))
    vehicleDocuments <- fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just registrationNo) onlyMandatoryDocs skipMessages
    let status =
          case find (\vehicleDoc -> vehicleDoc.registrationNo == registrationNo) vehicleDocuments of
            Just vehicleDoc -> computeAdminDocsVerificationStatus vehicleDoc.documents
            Nothing -> computeAdminDocsVerificationStatus []
    case status of
      DDVS.ADMIN_APPROVED -> pure ()
      DDVS.ADMIN_REJECTED -> do
        logWarning $ "validateMandatoryVehicleDocsForRC: mandatory vehicle docs rejected for RC " <> rc.id.getId
        throwError (InvalidRequest "Mandatory vehicle documents are rejected")
      DDVS.ADMIN_PENDING -> do
        logWarning $ "validateMandatoryVehicleDocsForRC: mandatory vehicle docs not approved for RC " <> rc.id.getId <> ", status: " <> show status
        throwError (InvalidRequest "Mandatory vehicle documents are not verified")

fetchVehicleDocuments ::
  IQuery.EntityImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  Maybe Text ->
  Maybe Bool ->
  Bool ->
  Flow [VehicleDocumentItem]
fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language Nothing onlyMandatoryDocs skipMessages = do
  -- All items required
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC entityImagesInfo allDocumentVerificationConfigs language Nothing onlyMandatoryDocs skipMessages
  processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC entityImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC Nothing onlyMandatoryDocs
  let processedVehicleDocuments = processedVehicleDocumentsWithoutRC <> processedVehicleDocumentsWithRC
  inprogressVehicleDocuments <- fetchInprogressVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language processedVehicleDocuments Nothing onlyMandatoryDocs skipMessages
  pure $ processedVehicleDocuments <> inprogressVehicleDocuments
fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs skipMessages = do
  -- Only one item required with specific registrationNo
  processedVehicleDocumentsWithRC <- fetchProcessedVehicleDocumentsWithRC entityImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs skipMessages
  let (entityTxt, entityId) = case entityImagesInfo.entity of
        IQuery.PersonEntity driver -> ("driverId", driver.id.getId)
        IQuery.VehicleRCEntity rc -> ("rcId", rc.id.getId)
  if null processedVehicleDocumentsWithRC
    then do
      processedVehicleDocumentsWithoutRC <- fetchProcessedVehicleDocumentsWithoutRC entityImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC (Just reqRegistrationNo) onlyMandatoryDocs
      if null processedVehicleDocumentsWithoutRC
        then do
          docs <- fetchInprogressVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language [] (Just reqRegistrationNo) onlyMandatoryDocs skipMessages
          if null docs
            then logWarning $ "No docs found for rcNo and " <> entityTxt <> ": " <> entityId
            else logInfo $ "Inprogress vehicle docs found for rcNo and " <> entityTxt <> ": " <> entityId
          pure docs
        else do
          logInfo $ "Processed vehicle docs without RC found for rcNo and " <> entityTxt <> ": " <> entityId
          pure processedVehicleDocumentsWithoutRC
    else do
      logInfo $ "Processed vehicle documents with RC found for rcNo and " <> entityTxt <> ": " <> entityId
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
      isVehicleSideDoc config = case config.documentCategory of
        Just DDVC.Vehicle -> True
        Just _ -> False
        Nothing -> config.documentType `elem` SDO.defaultVehicleDocumentTypes
      mandatoryVehicleDocumentVerificationConfigs =
        filter
          ( \config ->
              fromMaybe config.isMandatory config.isMandatoryForEnabling
                && config.vehicleCategory == vehicleCategory
                && isVehicleSideDoc config
          )
          allDocumentVerificationConfigs
  if onlyMandatoryDocs == Just True
    then do
      let vehicleDocumentTypes = nub (mandatoryVehicleDocumentVerificationConfigs <&> (.documentType))
      logInfo $
        "Fetch only mandatory vehicle docs types: merchantOpCityId: "
          <> merchantOpCityId.getId
          <> "; vehicleCategory: "
          <> show vehicleCategory
          <> "; vehicleDocumentTypes: "
          <> show vehicleDocumentTypes
      pure vehicleDocumentTypes
    else pure SDO.defaultVehicleDocumentTypes

fetchProcessedVehicleDocumentsWithRC ::
  IQuery.EntityImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  Maybe Text ->
  Maybe Bool ->
  Bool ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithRC entityImagesInfo allDocumentVerificationConfigs language mbReqRegistrationNo onlyMandatoryDocs skipMessages = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
  processedVehicles <- case entityImagesInfo.entity of
    IQuery.PersonEntity person -> do
      associations <- DRAQuery.findAllLinkedByDriverId person.id
      (catMaybes <$>) $
        forM associations $ \assoc -> do
          mbRc <- RCQuery.findById assoc.rcId
          -- filter by rcNo if required
          mbFilteredRc <- case mbRc of
            Just rc -> do
              rcCertificateNumber <- decrypt rc.certificateNumber
              let wrongRcNo = isJust mbReqRegistrationNo && Just rcCertificateNumber /= mbReqRegistrationNo
              return $ if wrongRcNo then Nothing else Just rc
            Nothing -> return Nothing
          return $ (assoc.isRcActive,assoc.rcId,) <$> mbFilteredRc
    IQuery.VehicleRCEntity rc -> do
      mbAssoc <- DRAQuery.findLatestLinkedByRCId rc.id entityImagesInfo.now
      pure [(maybe False (.isRcActive) mbAssoc, rc.id, rc)]
  processedVehicles `forM` \(isActive, rcId, processedVehicle) -> do
    rcImagesInfo <- IQuery.getRcImagesInfoFromEntityImagesInfo entityImagesInfo rcId vehicleDocsLoadedByRcId
    registrationNo <- decrypt processedVehicle.certificateNumber
    let dateOfUpload = processedVehicle.createdAt
    let verifiedVehicleCategory = DV.castVehicleVariantToVehicleCategory <$> processedVehicle.vehicleVariant
        userSelectedVehicleCategory = fromMaybe DVC.CAR $ processedVehicle.userPassedVehicleCategory <|> verifiedVehicleCategory
        docVerificationConfigs = filter (\config -> config.vehicleCategory == userSelectedVehicleCategory) allDocumentVerificationConfigs

    vehicleDocumentTypes <- getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs
    documents <-
      vehicleDocumentTypes `forM` \docType -> do
        (mbStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbMetadata) <- getProcessedVehicleDocuments entityImagesInfo docType processedVehicle (Just rcImagesInfo)
        case mbStatus of
          Just status -> do
            mbMessage <- documentStatusMessage status Nothing docType mbProcessedUrl language skipMessages
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> mbMessage, verificationUrl = mbProcessedUrl, s3Path = mbS3Path, imageId = mbImageId, imageId2 = Nothing, documentExpiry = mbExpiry, metadata = mbMetadata}
          Nothing -> do
            (status, mbReason, mbUrl, _, mbS3PathInProgress, mbImageIdInProgress) <- getInProgressVehicleDocuments entityImagesInfo (Just rcImagesInfo) docType docVerificationConfigs
            mbMessage <- documentStatusMessage status mbReason docType mbUrl language skipMessages
            return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbMessage, verificationUrl = mbUrl, s3Path = mbS3PathInProgress, imageId = mbImageIdInProgress, imageId2 = Nothing, documentExpiry = mbExpiry, metadata = Nothing}

    let mbRcImage = find (\img -> img.id == processedVehicle.documentImageId) entityImagesInfo.entityImages
        rcS3Path = mbRcImage <&> (.s3Path)
        rcImageId = Just processedVehicle.documentImageId.getId
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
          imageId = rcImageId,
          documentExpiry = rcExpiry,
          docsVerificationStatus = Just $ computeAdminDocsVerificationStatus documents
        }

fetchProcessedVehicleDocumentsWithoutRC ::
  IQuery.EntityImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Maybe Bool ->
  Flow [VehicleDocumentItem]
fetchProcessedVehicleDocumentsWithoutRC entityImagesInfo allDocumentVerificationConfigs processedVehicleDocumentsWithRC mbReqRegistrationNo onlyMandatoryDocs = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      mbPersonId = IQuery.getPersonEntityId entityImagesInfo
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
              return $ DocumentStatusItem {documentType = docType, verificationStatus = NO_DOC_AVAILABLE, verificationMessage = Nothing, verificationUrl = Nothing, s3Path = Nothing, imageId = Nothing, imageId2 = Nothing, documentExpiry = Nothing, metadata = Nothing}
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
                  imageId = Nothing,
                  documentExpiry = Nothing,
                  docsVerificationStatus = Nothing
                }
            ]
    Nothing -> return []

fetchInprogressVehicleDocuments ::
  IQuery.EntityImagesInfo ->
  [DVC.DocumentVerificationConfig] ->
  Language ->
  [VehicleDocumentItem] ->
  Maybe Text ->
  Maybe Bool ->
  Bool ->
  Flow [VehicleDocumentItem]
fetchInprogressVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language processedVehicleDocuments mbReqRegistrationNo onlyMandatoryDocs skipMessages = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
  mbVerificationReqRecord <- case entityImagesInfo.entity of
    IQuery.PersonEntity person -> do
      -- Driver inspection or status handler
      inprogressVehicleIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing person.id (docTypeToText DVC.VehicleRegistrationCertificate)
      inprogressVehicleHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing person.id DVC.VehicleRegistrationCertificate
      pure $ getLatestVerificationRecord inprogressVehicleIdfy inprogressVehicleHV
    IQuery.VehicleRCEntity _rc -> do
      -- Vehicle inspection
      -- Inprogress rc doc for Vehicle inspection case not required, because we already have processed rc
      pure Nothing
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
              -- VRC-derived "RC link already created (done)" check, replacing the
              -- driver_rc_association history read (findUnlinkedRC). Reuses the RC row
              -- fetched just above + transporterConfig from entityImagesInfo, so it adds
              -- no DB query: if the VRC exists, is not a fleet RC, and
              -- canCreateRCAssociation holds, the driver-RC link would have been created,
              -- so this is not an in-progress document.
              let mbRcId = (.id) <$> rc
                  rcLinkAlreadyCreated = maybe False (\rc_ -> isNothing rc_.fleetOwnerId && SDO.canCreateRCAssociation entityImagesInfo.transporterConfig rc_) rc
              mbRcImagesInfo <- forM mbRcId $ \rcId -> do
                IQuery.getRcImagesInfoFromEntityImagesInfo entityImagesInfo rcId vehicleDocsLoadedByRcId
              if isJust (find (\doc -> doc.registrationNo == registrationNo) processedVehicleDocuments) || rcLinkAlreadyCreated
                then return []
                else do
                  let userSelectedVehicleCategory = fromMaybe DVC.CAR verificationReqRecord.vehicleCategory
                      verifiedVehicleCategory = Nothing
                      docVerificationConfigs = filter (\config -> config.vehicleCategory == userSelectedVehicleCategory) allDocumentVerificationConfigs

                  vehicleDocumentTypes <- getVehicleDocTypes merchantOpCityId allDocumentVerificationConfigs verifiedVehicleCategory userSelectedVehicleCategory onlyMandatoryDocs
                  documents <-
                    vehicleDocumentTypes `forM` \docType -> do
                      (status, mbReason, mbUrl, _, mbS3Path, mbImageId) <- getInProgressVehicleDocuments entityImagesInfo mbRcImagesInfo docType docVerificationConfigs
                      mbMessage <- documentStatusMessage status mbReason docType mbUrl language skipMessages
                      return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbMessage, verificationUrl = mbUrl, s3Path = mbS3Path, imageId = mbImageId, imageId2 = Nothing, documentExpiry = Nothing, metadata = Nothing}
                  let mbRcIdText = (.getId) <$> mbRcId
                      mbRcImage =
                        find
                          (\img -> img.imageType == DVC.VehicleRegistrationCertificate && (isNothing mbRcIdText || img.rcId == mbRcIdText))
                          entityImagesInfo.entityImages
                      rcS3Path = mbRcImage <&> (.s3Path)
                      rcImageId = (.id.getId) <$> mbRcImage
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
                          imageId = rcImageId,
                          documentExpiry = Nothing,
                          docsVerificationStatus = Nothing
                        }
                    ]
    Nothing -> return []

computeAdminDocsVerificationStatus :: [DocumentStatusItem] -> DDVS.DocsVerificationStatus
computeAdminDocsVerificationStatus docs
  | null docs = DDVS.ADMIN_PENDING
  | any ((`elem` [INVALID, UNAUTHORIZED]) . (.verificationStatus)) docs = DDVS.ADMIN_REJECTED
  | all ((== VALID) . (.verificationStatus)) docs = DDVS.ADMIN_APPROVED
  | otherwise = DDVS.ADMIN_PENDING

getProcessedVehicleDocuments :: IQuery.EntityImagesInfo -> DVC.DocumentType -> RC.VehicleRegistrationCertificate -> Maybe IQuery.RcImagesInfo -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe DocumentMetadata)
getProcessedVehicleDocuments entityImagesInfo docType vehicleRC mbRcImagesInfo = do
  let entity = entityImagesInfo.entity
      (mbS3Path, mbImageId) = getImageMetaFromVehicleImage entityImagesInfo docType mbRcImagesInfo
      enableMetadata = fromMaybe False entityImagesInfo.transporterConfig.enableDocumentMetadata
      lookupImage imgId =
        let mbImg = find (\img -> img.id == imgId) entityImagesInfo.entityImages
            s3 = mbImg <&> (.s3Path)
            iid = Just imgId.getId
         in (s3, iid)
  case docType of
    DVC.VehicleRegistrationCertificate -> do
      let status = mapStatus vehicleRC.verificationStatus
          reason
            | status == INVALID && not (null vehicleRC.failedRules) = Just $ T.intercalate ", " vehicleRC.failedRules
            | otherwise = vehicleRC.rejectReason
          (s3, iid) = lookupImage vehicleRC.documentImageId
      mbMetadata <-
        if enableMetadata
          then do
            vehicleNumberPlate <- decrypt vehicleRC.certificateNumber
            pure $
              Just $
                RCMetadata
                  RCDocumentMetadata
                    { fitnessExpiry = vehicleRC.fitnessExpiry,
                      vehicleNumberPlate,
                      vehicleVariant = show <$> vehicleRC.vehicleVariant,
                      vehicleManufacturer = vehicleRC.vehicleManufacturer,
                      vehicleModel = vehicleRC.vehicleModel,
                      vehicleModelYear = vehicleRC.vehicleModelYear,
                      vehicleColor = vehicleRC.vehicleColor
                    }
          else pure Nothing
      return (Just status, reason, Nothing, Just vehicleRC.fitnessExpiry, s3, iid, mbMetadata)
    DVC.VehiclePermit -> do
      mbDoc <- listToMaybe <$> VPQuery.findByRcId (Just 1) Nothing vehicleRC.id
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId)) mbDoc
      mbMetadata <-
        if enableMetadata
          then forM mbDoc $ \doc -> do
            pNo <- decrypt doc.permitNumber
            rcNo <- decrypt vehicleRC.certificateNumber
            pure $
              VehiclePermitMetadata
                VehiclePermitDocumentMetadata
                  { permitNumber = pNo,
                    permitExpiry = doc.permitExpiry,
                    regionCovered = doc.regionCovered,
                    rcNumber = rcNo
                  }
          else pure Nothing
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, vehicleRC.permitExpiry, s3, iid, mbMetadata)
    DVC.VehicleFitnessCertificate -> do
      mbDoc <- listToMaybe <$> VFCQuery.findByRcId (Just 1) Nothing vehicleRC.id
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId)) mbDoc
      mbMetadata <-
        if enableMetadata
          then forM mbDoc $ \doc -> do
            appNo <- decrypt doc.applicationNumber
            rcNo <- decrypt vehicleRC.certificateNumber
            pure $
              VehicleFitnessMetadata
                VehicleFitnessCertificateDocumentMetadata
                  { fitnessExpiry = doc.fitnessExpiry,
                    applicationNumber = appNo,
                    rcNumber = rcNo
                  }
          else pure Nothing
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, Just vehicleRC.fitnessExpiry, s3, iid, mbMetadata)
    DVC.VehicleInsurance -> do
      mbDoc <- listToMaybe <$> VIQuery.findByRcId (Just 1) Nothing vehicleRC.id
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId)) mbDoc
      mbMetadata <-
        if enableMetadata
          then forM mbDoc $ \doc -> do
            polNo <- decrypt doc.policyNumber
            rcNo <- decrypt vehicleRC.certificateNumber
            pure $
              VehicleInsuranceMetadata
                VehicleInsuranceDocumentMetadata
                  { policyNumber = polNo,
                    insuranceExpiry = doc.policyExpiry,
                    insuranceProvider = doc.policyProvider,
                    rcNumber = rcNo
                  }
          else pure Nothing
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), (mbDoc >>= (.rejectReason)), Nothing, vehicleRC.insuranceValidity, s3, iid, mbMetadata)
    DVC.VehiclePUC -> do
      mbDoc <- listToMaybe <$> VPUCQuery.findByRcId (Just 1) Nothing vehicleRC.id
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId)) mbDoc
      mbMetadata <-
        if enableMetadata
          then forM mbDoc $ \doc -> do
            mbPucNo <- mapM decrypt doc.pucNumber
            pure $
              VehiclePUCMetadata
                VehiclePUCDocumentMetadata
                  { pucNumber = mbPucNo,
                    pucExpiry = doc.pucExpiry
                  }
          else pure Nothing
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, vehicleRC.pucExpiry, s3, iid, mbMetadata)
    DVC.VehicleNOC -> do
      mbDoc <- listToMaybe <$> VNOCQuery.findByRcId (Just 1) Nothing vehicleRC.id
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId)) mbDoc
      return (mapStatus <$> (mbDoc <&> (.verificationStatus)), Nothing, Nothing, mbDoc <&> (.nocExpiry), s3, iid, Nothing)
    DVC.VehicleInspectionForm -> do
      -- Check all vehicle photos based on RC, not driver
      (status, reason, url) <- checkVehiclePhotosStatusByRC mbRcImagesInfo
      return (Just status, reason, url, Nothing, Nothing, Nothing, Nothing)
    DVC.InspectionHub -> do
      registrationNo <- decrypt vehicleRC.certificateNumber
      (status, reason) <- getInspectionHubStatusAndReason DOHR.ONBOARDING_INSPECTION Nothing (Just registrationNo)
      return (status, reason, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.BotApproval -> do
      registrationNo <- decrypt vehicleRC.certificateNumber
      status <- getBotApprovalStatusForVehicle vehicleRC.id.getId registrationNo
      return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.SubscriptionPlan -> case entity of
      IQuery.PersonEntity person -> do
        mbPlan <- QDPlan.findByDriverIdWithServiceName (cast person.id) Plan.YATRI_SUBSCRIPTION -- fix later on basis of vehicle category
        return (Just $ boolToStatus (isJust mbPlan), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      IQuery.VehicleRCEntity _rc -> return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    _ -> do
      let mbLatestImage = case mbRcImagesInfo of
            Just rcImagesInfo -> listToMaybe (IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType)
            Nothing -> Nothing
          mbStatus = mbLatestImage >>= (.verificationStatus) <&> mapStatus
      return (mbStatus, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing)
  where
    boolToStatus :: Bool -> ResponseStatus
    boolToStatus = \case
      True -> VALID
      False -> NO_DOC_AVAILABLE

getImageMetaFromLatestImage :: IQuery.EntityImagesInfo -> DVC.DocumentType -> (Maybe Text, Maybe Text)
getImageMetaFromLatestImage entityImagesInfo docType =
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
      mbLatestImage = listToMaybe images
   in (mbLatestImage <&> (.s3Path), mbLatestImage <&> (.id.getId))

getImageMetaFromVehicleImage :: IQuery.EntityImagesInfo -> DVC.DocumentType -> Maybe IQuery.RcImagesInfo -> (Maybe Text, Maybe Text)
getImageMetaFromVehicleImage entityImagesInfo docType mbRcImagesInfo =
  let images = case docType of
        DVC.VehicleRegistrationCertificate ->
          let merchantId = entityImagesInfo.merchantOperatingCity.merchantId
           in IQuery.filterImagesByEntityAndType entityImagesInfo merchantId docType
        _ -> case mbRcImagesInfo of
          Just rcImagesInfo -> IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
          Nothing -> []
      mbLatestImage = listToMaybe images
   in (mbLatestImage <&> (.s3Path), mbLatestImage <&> (.id.getId))

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

vehicleDocsLoadedByRcId :: [DVC.DocumentType]
vehicleDocsLoadedByRcId =
  vehicleDocsByRcIdList
    <> [ DVC.VehiclePermit,
         DVC.VehicleFitnessCertificate,
         DVC.VehicleInsurance,
         DVC.VehiclePUC,
         DVC.VehicleNOC
       ]

getInProgressVehicleDocuments :: IQuery.EntityImagesInfo -> Maybe IQuery.RcImagesInfo -> DVC.DocumentType -> [DVC.DocumentVerificationConfig] -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text)
getInProgressVehicleDocuments entityImagesInfo mbRcImagesInfo docType docVerificationConfigs = do
  let onlyImageLookup = maybe False (fromMaybe False . (.onlyImageVerificationStatusLookupRequired)) $ find (\c -> c.documentType == docType) docVerificationConfigs
      (mbS3Path, mbImageId) = case mbRcImagesInfo of
        Just rcImagesInfo ->
          let images = IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
              mbLatestImage = listToMaybe images
           in (mbLatestImage <&> (.s3Path), mbLatestImage <&> (.id.getId))
        Nothing -> getImageMetaFromLatestImage entityImagesInfo docType
  (status, mbReason, mbUrl) <- case docType of
    DVC.VehicleRegistrationCertificate -> checkIfUnderProgress entityImagesInfo DVC.VehicleRegistrationCertificate
    DVC.SubscriptionPlan -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    DVC.VehiclePermit -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo DVC.VehiclePermit onlyImageLookup
    DVC.VehicleFitnessCertificate -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo DVC.VehicleFitnessCertificate onlyImageLookup
    DVC.VehicleInsurance -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo DVC.VehicleInsurance onlyImageLookup
    DVC.VehiclePUC -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo DVC.VehiclePUC onlyImageLookup
    DVC.VehicleInspectionForm -> checkVehiclePhotosStatusByRC mbRcImagesInfo
    DVC.VehicleNOC -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo DVC.VehicleNOC onlyImageLookup
    DVC.InspectionHub -> do
      mbRegistrationNo <- case mbRcImagesInfo of
        Just rcImagesInfo -> do
          mbRc <- RCQuery.findById rcImagesInfo.rcId
          case mbRc of
            Just rc -> Just <$> decrypt rc.certificateNumber
            Nothing -> return Nothing
        Nothing -> return Nothing
      case mbRegistrationNo of
        Just registrationNo -> do
          (mbStatus, reason) <- getInspectionHubStatusAndReason DOHR.ONBOARDING_INSPECTION Nothing (Just registrationNo)
          let status = fromMaybe INVALID mbStatus
          return (status, reason, Nothing)
        Nothing -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    DVC.BotApproval -> do
      mbRc <- case mbRcImagesInfo of
        Just rcImagesInfo -> RCQuery.findById rcImagesInfo.rcId
        Nothing -> return Nothing
      case mbRc of
        Just rc -> do
          registrationNo <- decrypt rc.certificateNumber
          mbStatus <- getBotApprovalStatusForVehicle rc.id.getId registrationNo
          return (fromMaybe NO_DOC_AVAILABLE mbStatus, Nothing, Nothing)
        Nothing -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    _ | docType `elem` vehicleDocsByRcIdList -> return $ checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo docType onlyImageLookup
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
  return (status, mbReason, mbUrl, Nothing, mbS3Path, mbImageId)

checkIfImageUploadedOrInvalidatedByRC :: Maybe IQuery.RcImagesInfo -> DDVC.DocumentType -> Bool -> (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidatedByRC mbRcImagesInfo docType onlyImageLookup = do
  let images = case mbRcImagesInfo of
        Just rcImagesInfo -> IQuery.filterRecentByPersonRCAndImageType rcImagesInfo docType
        _ -> []
  case images of
    [] -> (NO_DOC_AVAILABLE, Nothing, Nothing)
    latestImage : _ -> do
      case latestImage.verificationStatus of
        Just Documents.VALID | onlyImageLookup -> (VALID, Nothing, Nothing)
        Just Documents.INVALID -> (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        _ -> (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

checkIfUnderProgress :: IQuery.EntityImagesInfo -> DVC.DocumentType -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfUnderProgress entityImagesInfo docType = do
  mbVerificationReqRecord <- case entityImagesInfo.entity of
    IQuery.PersonEntity person -> do
      -- Driver inspection or status handler: docType: DriverLicense/VehicleRegistrationCertificate
      let driverId = person.id
      mbVerificationReqIdfy <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId (docTypeToText docType)
      mbVerificationReqHV <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId docType
      pure $ getLatestVerificationRecord mbVerificationReqIdfy mbVerificationReqHV
    IQuery.VehicleRCEntity _rc -> do
      -- Vehicle inspection: docType: VehicleRegistrationCertificate
      -- Inprogress rc doc for Vehicle inspection case not required, because we already have processed rc
      pure Nothing
  case mbVerificationReqRecord of
    Just verificationReqRecord -> do
      if verificationReqRecord.status == "pending" || verificationReqRecord.status == "source_down_retrying"
        then return (PENDING, Nothing, Nothing)
        else return (FAILED, verificationReqRecord.verificaitonResponse, Nothing)
    Nothing -> do
      let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
      handleImages entityImagesInfo.transporterConfig.onboardingTryLimit images
  where
    handleImages onboardingTryLimit images
      | null images = return (NO_DOC_AVAILABLE, Nothing, Nothing)
      | length images > onboardingTryLimit * bool 1 2 (docType == DVC.DriverLicense || docType == DVC.AadhaarCard) = return (LIMIT_EXCEED, Nothing, Nothing)
      | otherwise = do
        let latestImage = head images
        if latestImage.verificationStatus == Just Documents.INVALID
          then return (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
          else return (NO_DOC_AVAILABLE, Nothing, Nothing)

-- | Blank/whitespace-only reject reason -> Nothing, so the status read falls through to the next fallback (image reason / translated status) instead of a blank message.
nonEmptyReason :: Text -> Maybe Text
nonEmptyReason t = if T.null (T.strip t) then Nothing else Just t

extractImageFailReason :: Maybe DriverOnboardingError -> Maybe Text
extractImageFailReason imageError =
  case imageError of
    Just (ImageNotValid reason) -> nonEmptyReason reason -- manual reject reason from the update-documents dashboard api
    Just FaceMatchFailed -> toMessage FaceMatchFailed -- surfaces the face-match failure in the status API's verificationMessage
    _ -> Nothing

documentStatusMessage :: ResponseStatus -> Maybe Text -> DDVC.DocumentType -> Maybe BaseUrl -> Language -> Bool -> Flow (Maybe Text)
documentStatusMessage status mbReason docType mbVerificationUrl language skipMessages
  | skipMessages = pure Nothing
  | otherwise = do
    msg <- case (status, docType, mbVerificationUrl) of
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
    pure $ Just msg

mapStatus :: Documents.VerificationStatus -> ResponseStatus
mapStatus = \case
  Documents.PENDING -> PENDING
  Documents.MANUAL_VERIFICATION_REQUIRED -> MANUAL_VERIFICATION_REQUIRED
  Documents.VALID -> VALID
  Documents.INVALID -> INVALID
  Documents.UNAUTHORIZED -> UNAUTHORIZED
  Documents.PULL_REQUIRED -> PULL_REQUIRED

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
  mTranslation <- getConfig (TranslationDimensions {merchantOperatingCityId = Nothing, messageKey = key, language = Just lang}) (Just (MTQuery.findByErrorAndLanguage key lang))
  return $ fromMaybe key (mTranslation <&> (.message))

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- getConfig (TranslationDimensions {merchantOperatingCityId = Nothing, messageKey = T.pack (show msg), language = Just lang}) (Just (MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang))
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"

getLatestVerificationRecord :: Maybe IV.IdfyVerification -> Maybe HV.HyperVergeVerification -> Maybe SDO.VerificationReqRecord
getLatestVerificationRecord mbIdfyVerificationReq mbHvVerificationReq = do
  case (mbIdfyVerificationReq <&> (.createdAt), mbHvVerificationReq <&> (.createdAt)) of
    (Just idfyCreatedAt, Just hvCreatedAt) -> if idfyCreatedAt > hvCreatedAt then (SDO.makeIdfyVerificationReqRecord =<< mbIdfyVerificationReq) <|> (SDO.makeHVVerificationReqRecord <$> mbHvVerificationReq) else SDO.makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Nothing, Just _) -> SDO.makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Just _, Nothing) -> SDO.makeIdfyVerificationReqRecord =<< mbIdfyVerificationReq
    (Nothing, Nothing) -> Nothing

-- Convert CommonDriverOnboardingDocuments to CommonDocumentItem

findLatestInspectionHubRequest :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DOHR.RequestType -> Maybe (Id DP.Person) -> Maybe Text -> m (Maybe DOHR.OperationHubRequests)
findLatestInspectionHubRequest requestType mbDriverId mbRegistrationNo = case requestType of
  DOHR.DRIVER_ONBOARDING_INSPECTION -> case mbDriverId of
    Just driverId -> runInReplica $ QOHRE.findLatestByDriverIdAndRequestType driverId requestType
    Nothing -> pure Nothing
  DOHR.ONBOARDING_INSPECTION -> case mbRegistrationNo of
    Just registrationNo -> runInReplica $ QOHRE.findLatestByRegistrationNoAndRequestType registrationNo requestType
    Nothing -> pure Nothing
  _ -> pure Nothing

checkInspectionHubRequestCreated :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DOHR.RequestType -> Maybe (Id DP.Person) -> Maybe Text -> m (Maybe DOHR.RequestStatus)
checkInspectionHubRequestCreated requestType mbDriverId mbRegistrationNo =
  ((.requestStatus) <$>) <$> findLatestInspectionHubRequest requestType mbDriverId mbRegistrationNo

mapInspectionHubRequestStatusToResponseStatus :: Maybe DOHR.RequestStatus -> Maybe ResponseStatus
mapInspectionHubRequestStatusToResponseStatus mbRequestStatus = case mbRequestStatus of
  Just DOHR.APPROVED -> Just VALID
  Just DOHR.PENDING -> Just PENDING
  Just DOHR.REJECTED -> Just INVALID
  Nothing -> Just NO_DOC_AVAILABLE

-- | Inspection-hub status plus the operator's remarks (the rejection reason surfaced to the status APIs).
getInspectionHubStatusAndReason :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DOHR.RequestType -> Maybe (Id DP.Person) -> Maybe Text -> m (Maybe ResponseStatus, Maybe Text)
getInspectionHubStatusAndReason requestType mbDriverId mbRegistrationNo = do
  mbReq <- findLatestInspectionHubRequest requestType mbDriverId mbRegistrationNo
  pure (mapInspectionHubRequestStatusToResponseStatus ((.requestStatus) <$> mbReq), (mbReq >>= (.remarks)) >>= nonEmptyReason)

-- | BotApproval status, from the latest BOT_REVIEW ReviewRequest for the entity:
--   COMPLETED -> VALID, IN_PROGRESS -> PENDING, REJECTED -> INVALID, none -> NO_DOC_AVAILABLE.
mapReviewRequestStatusToResponseStatus :: Maybe DRR.RequestStatus -> Maybe ResponseStatus
mapReviewRequestStatusToResponseStatus = \case
  Just DRR.COMPLETED -> Just VALID
  Just DRR.IN_PROGRESS -> Just PENDING
  Just DRR.REJECTED -> Just INVALID
  Nothing -> Just NO_DOC_AVAILABLE

-- | BotApproval for a driver / fleet-owner (person-keyed): FLEET_OWNER/FLEET_BUSINESS -> FLEET_OWNER, else DRIVER.
getBotApprovalStatusForPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Role -> Id DP.Person -> m (Maybe ResponseStatus)
getBotApprovalStatusForPerson role personId = do
  let entityType = if role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] then DRR.FLEET_OWNER else DRR.DRIVER
  mbReq <- SQRR.findLatestByEntityAndType personId.getId entityType DRR.BOT_REVIEW Nothing
  pure $ mapReviewRequestStatusToResponseStatus ((.requestStatus) <$> mbReq)

-- | BotApproval for a vehicle (RC-keyed): entityId = rc id, rcNo = registration number.
getBotApprovalStatusForVehicle :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Text -> m (Maybe ResponseStatus)
getBotApprovalStatusForVehicle rcId registrationNo = do
  mbReq <- SQRR.findLatestByEntityAndType rcId DRR.VEHICLE DRR.BOT_REVIEW (Just registrationNo)
  pure $ mapReviewRequestStatusToResponseStatus ((.requestStatus) <$> mbReq)
