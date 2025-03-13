-- TODO remove
{-# OPTIONS_GHC -Wno-unused-imports #-}

module SharedLogic.DriverOnboarding.Status where

-- ()

import Control.Applicative ((<|>))
import qualified Control.Monad.Extra as Extra
import qualified Data.Text as T
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
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as Plan
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import qualified Domain.Types.VehicleVariant as DV
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import qualified SharedLogic.DriverOnboarding as Common
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as BVQuery
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverSSN as QDSSN
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import Storage.Queries.Person as Person
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as Vehicle
import qualified Storage.Queries.VehicleFitnessCertificate as VFCQuery
import qualified Storage.Queries.VehicleInsurance as VIQuery
import qualified Storage.Queries.VehiclePUC as VPUCQuery
import qualified Storage.Queries.VehiclePermit as VPQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.BackgroundVerification as BackgroundVerification
import Tools.Error (DriverOnboardingError (ImageNotValid))
import Utils.Common.Cac.KeyNameConstants

data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED | UNAUTHORIZED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

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

-- TODO correct order

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
    mbVehicle <- Vehicle.findById personId
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

fetchDriverDocuments ::
  Id DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow [DocumentStatusItem]
fetchDriverDocuments personId merchantOpCity transporterConfig language = do
  let merchantId = merchantOpCity.merchantId
      merchantOpCityId = merchantOpCity.id
  Common.driverDocumentTypes `forM` \docType -> do
    (mbStatus, mbProcessedReason, mbProcessedUrl) <- getProcessedDriverDocuments docType personId merchantId merchantOpCityId
    case mbStatus of
      Just status -> do
        message <- documentStatusMessage status Nothing docType mbProcessedUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = mbProcessedReason <|> Just message, verificationUrl = mbProcessedUrl}
      Nothing -> do
        (status, mbReason, mbUrl) <- getInProgressDriverDocuments docType personId transporterConfig.onboardingTryLimit merchantId merchantOpCityId
        message <- documentStatusMessage status mbReason docType mbUrl language
        return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message, verificationUrl = mbUrl}

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

getProcessedDriverDocuments :: DVC.DocumentType -> Id SP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
getProcessedDriverDocuments docType driverId _merchantId _merchantOpCityId =
  case docType of
    DVC.DriverLicense -> do
      mbDL <- DLQuery.findByDriverId driverId -- add failure reason in dl and rc
      return (mapStatus <$> (mbDL <&> (.verificationStatus)), mbDL >>= (.rejectReason), Nothing)
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
    DVC.BackgroundVerification -> do
      mbBackgroundVerification <- BVQuery.findByDriverId driverId
      if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
        then return (Just VALID, Nothing, Nothing)
        else return (Nothing, Nothing, Nothing)
    _ -> return (Nothing, Nothing, Nothing)

getProcessedVehicleDocuments :: DVC.DocumentType -> Id SP.Person -> RC.VehicleRegistrationCertificate -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
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

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"

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
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      mbImages <- IQuery.findRecentLatestByPersonIdAndImageType driverId DDVC.ProfilePhoto
      return (fromMaybe NO_DOC_AVAILABLE (mapStatus <$> (mbImages >>= (.verificationStatus))), Nothing, Nothing)
    DDVC.UploadProfile -> checkIfImageUploadedOrInvalidated DDVC.UploadProfile driverId
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

getInProgressVehicleDocuments :: DVC.DocumentType -> Id SP.Person -> Int -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
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

checkIfUnderProgress :: DVC.DocumentType -> Id SP.Person -> Int -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
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
      | length images > onboardingTryLimit = return (LIMIT_EXCEED, Nothing, Nothing)
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

mapStatus :: Documents.VerificationStatus -> ResponseStatus
mapStatus = \case
  Documents.PENDING -> PENDING
  Documents.MANUAL_VERIFICATION_REQUIRED -> MANUAL_VERIFICATION_REQUIRED
  Documents.VALID -> VALID
  Documents.INVALID -> INVALID
  Documents.UNAUTHORIZED -> UNAUTHORIZED

checkImageValidity :: DVC.DocumentType -> Id SP.Person -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
checkImageValidity docType driverId = do
  validImages <- IQuery.findImageByPersonIdAndImageTypeAndVerificationStatus driverId docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == (Just Documents.VALID)) validImages = return (Just VALID, Nothing, Nothing)
      | any (\img -> img.verificationStatus == (Just Documents.MANUAL_VERIFICATION_REQUIRED)) validImages = return (Just MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
      | otherwise = return (Nothing, Nothing, Nothing)

checkBackgroundVerificationStatus :: Id SP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
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

getLatestVerificationRecord :: Maybe IV.IdfyVerification -> Maybe HV.HyperVergeVerification -> Maybe VerificationReqRecord
getLatestVerificationRecord mbIdfyVerificationReq mbHvVerificationReq = do
  case (mbIdfyVerificationReq <&> (.createdAt), mbHvVerificationReq <&> (.createdAt)) of
    (Just idfyCreatedAt, Just hvCreatedAt) -> if idfyCreatedAt > hvCreatedAt then makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq else makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Nothing, Just _) -> makeHVVerificationReqRecord <$> mbHvVerificationReq
    (Just _, Nothing) -> makeIdfyVerificationReqRecord <$> mbIdfyVerificationReq
    (Nothing, Nothing) -> Nothing
