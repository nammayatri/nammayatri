{-# LANGUAGE DerivingStrategies #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusHandler,
    mapStatus,
    verificationStatusWithMessage,
  )
where

-- temp import for backward compatibility should be removed later

import qualified Control.Monad.Extra as Extra
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Plan as DAPlan
import Domain.Types.AadhaarVerification as AV
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLicense as DL
import qualified Domain.Types.IdfyVerification as IV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as Plan
import qualified Domain.Types.Vehicle as DVeh
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import qualified Storage.Queries.AadhaarVerification as SAV
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverSSN as QDSSN
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
import Utils.Common.Cac.KeyNameConstants

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- INVALID is the state
--   which the doc switches to when, for example, it's expired.
data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus, -- deprecated
    dlVerficationMessage :: Text, -- deprecated
    rcVerificationStatus :: ResponseStatus, -- deprecated
    rcVerficationMessage :: Text, -- deprecated
    aadhaarVerificationStatus :: ResponseStatus, -- deprecated

    --- use these fields
    driverDocuments :: [DocumentStatusItem],
    vehicleDocuments :: [VehicleDocumentItem],
    enabled :: Bool
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data VehicleDocumentItem = VehicleDocumentItem
  { registrationNo :: Text,
    userSelectedVehicleCategory :: DVeh.Category,
    verifiedVehicleCategory :: Maybe DVeh.Category,
    isVerified :: Bool,
    isActive :: Bool,
    vehicleModel :: Maybe Text,
    documents :: [DocumentStatusItem]
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

data DocumentStatusItem = DocumentStatusItem
  { documentType :: DVC.DocumentType,
    verificationStatus :: ResponseStatus,
    verificationMessage :: Maybe Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Flow StatusRes
statusHandler (personId, merchantId, merchantOpCityId) multipleRC = do
  -- multipleRC flag is temporary to support backward compatibility
  person <- runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  merchantOperatingCity <- SMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  let language = fromMaybe merchantOperatingCity.language person.language
  (dlStatus, mDL, dlVerficationMessage) <- getDLAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit merchantOperatingCity.language
  (rcStatus, _, rcVerficationMessage) <- getRCAndStatus personId merchantOpCityId transporterConfig.onboardingTryLimit multipleRC merchantOperatingCity.language
  (aadhaarStatus, _) <- getAadhaarStatus personId

  driverDocuments <-
    driverDocumentTypes `forM` \docType -> do
      mbStatus <- getProcessedDriverDocuments docType personId
      case mbStatus of
        Just status -> do
          message <- documentStatusMessage status Nothing docType language
          return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message}
        Nothing -> do
          (status, mbReason) <- getInProgressDriverDocuments docType personId transporterConfig.onboardingTryLimit
          message <- documentStatusMessage status mbReason docType language
          return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message}

  processedVehicleDocumentsWithRC <- do
    processedVehicles <- do
      associations <- DRAQuery.findAllLinkedByDriverId personId
      if null associations
        then return []
        else (associations `forM` (\assoc -> RCQuery.findById assoc.rcId >>= (\rc -> return $ (assoc.isRcActive,) <$> rc))) >>= (return . catMaybes)
    processedVehicles `forM` \(isActive, processedVehicle) -> do
      registrationNo <- decrypt processedVehicle.certificateNumber
      documents <-
        vehicleDocumentTypes `forM` \docType -> do
          mbStatus <- getProcessedVehicleDocuments docType personId processedVehicle
          case mbStatus of
            Just status -> do
              message <- documentStatusMessage status Nothing docType language
              return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message}
            Nothing -> do
              (status, mbReason) <- getInProgressVehicleDocuments docType personId transporterConfig.onboardingTryLimit
              message <- documentStatusMessage status mbReason docType language
              return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message}
      return $
        VehicleDocumentItem
          { registrationNo,
            userSelectedVehicleCategory = fromMaybe (maybe DVeh.CAR getCategory processedVehicle.vehicleVariant) processedVehicle.userPassedVehicleCategory,
            verifiedVehicleCategory = getCategory <$> processedVehicle.vehicleVariant,
            isVerified = False,
            isActive,
            vehicleModel = processedVehicle.vehicleModel,
            documents
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
                return $ DocumentStatusItem {documentType = docType, verificationStatus = NO_DOC_AVAILABLE, verificationMessage = Nothing}
            return $
              [ VehicleDocumentItem
                  { registrationNo = vehicle.registrationNo,
                    userSelectedVehicleCategory = getCategory vehicle.variant,
                    verifiedVehicleCategory = Just $ getCategory vehicle.variant,
                    isVerified = True,
                    isActive = True,
                    vehicleModel = Just vehicle.model,
                    documents
                  }
              ]
      Nothing -> return []

  let processedVehicleDocuments = processedVehicleDocumentsWithoutRC <> processedVehicleDocumentsWithRC
  inprogressVehicleDocuments <- do
    inprogressVehicle <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing personId DVC.VehicleRegistrationCertificate
    case inprogressVehicle of
      Just verificationReq -> do
        registrationNoEither <- try @_ @SomeException (decrypt verificationReq.documentNumber)
        case registrationNoEither of
          Left err -> do
            logError $ "Error while decrypting document number: " <> (verificationReq.documentNumber & unEncrypted . encrypted) <> " with err: " <> show err
            return []
          Right registrationNo -> do
            if isJust $ find (\doc -> doc.registrationNo == registrationNo) processedVehicleDocuments
              then return []
              else do
                documents <-
                  vehicleDocumentTypes `forM` \docType -> do
                    (status, mbReason) <- getInProgressVehicleDocuments docType personId transporterConfig.onboardingTryLimit
                    message <- documentStatusMessage status mbReason docType language
                    return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = Just message}
                return $
                  [ VehicleDocumentItem
                      { registrationNo,
                        userSelectedVehicleCategory = fromMaybe DVeh.CAR verificationReq.vehicleCategory,
                        verifiedVehicleCategory = Nothing,
                        isVerified = False,
                        isActive = False,
                        vehicleModel = Nothing,
                        documents
                      }
                  ]
      Nothing -> return []

  let vehicleDocumentsUnverified = processedVehicleDocuments <> inprogressVehicleDocuments
  -- check if driver is enabled if not then if all mandatory docs are verified then enable the driver
  vehicleDocuments <-
    vehicleDocumentsUnverified `forM` \vehicleDoc@VehicleDocumentItem {..} -> do
      allVehicleDocsVerified <- Extra.allM (\doc -> checkIfDocumentValid merchantOpCityId doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus) vehicleDoc.documents
      allDriverDocsVerified <- Extra.allM (\doc -> checkIfDocumentValid merchantOpCityId doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus) driverDocuments
      when (allVehicleDocsVerified && allDriverDocsVerified) $ enableDriver merchantOpCityId personId mDL
      mbVehicle <- Vehicle.findById personId -- check everytime
      when (isNothing mbVehicle && allVehicleDocsVerified && allDriverDocsVerified && isNothing multipleRC) $
        activateRCAutomatically personId merchantId merchantOpCityId vehicleDoc.registrationNo
      if allVehicleDocsVerified then return $ VehicleDocumentItem {isVerified = True, ..} else return vehicleDoc

  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  return $
    StatusRes
      { dlVerificationStatus = dlStatus,
        dlVerficationMessage = dlVerficationMessage,
        rcVerificationStatus = rcStatus,
        rcVerficationMessage = rcVerficationMessage,
        aadhaarVerificationStatus = aadhaarStatus,
        driverDocuments,
        vehicleDocuments,
        enabled = driverInfo.enabled
      }

enableDriver :: Id DMOC.MerchantOperatingCity -> Id SP.Person -> Maybe DL.DriverLicense -> Flow ()
enableDriver _ _ Nothing = return ()
enableDriver merchantOpCityId personId (Just dl) = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  unless driverInfo.enabled $ do
    enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId True
    whenJust dl.driverName $ \name -> Person.updateName name personId

activateRCAutomatically :: Id SP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Flow ()
activateRCAutomatically personId merchantId merchantOpCityId rcNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantId, merchantOpCityId) rcStatusReq

checkIfDocumentValid :: Id DMOC.MerchantOperatingCity -> DVC.DocumentType -> DVeh.Category -> ResponseStatus -> Flow Bool
checkIfDocumentValid merchantOperatingCityId docType category status = do
  mbVerificationConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOperatingCityId docType category
  case mbVerificationConfig of
    Just verificationConfig -> do
      if verificationConfig.isMandatory
        then case status of
          VALID -> return True
          MANUAL_VERIFICATION_REQUIRED -> return verificationConfig.isDefaultEnabledOnManualVerification
          _ -> return False
        else return True
    Nothing -> return True

getProcessedDriverDocuments :: DVC.DocumentType -> Id SP.Person -> Flow (Maybe ResponseStatus)
getProcessedDriverDocuments docType driverId =
  case docType of
    DVC.DriverLicense -> do
      mbDL <- DLQuery.findByDriverId driverId
      return $ mapStatus <$> (mbDL <&> (.verificationStatus))
    DVC.AadhaarCard -> do
      mbAadhaarCard <- SAV.findByDriverId driverId
      return $ boolToStatus <$> (mbAadhaarCard <&> (.isVerified))
    DVC.Permissions -> return $ Just VALID
    DVC.SocialSecurityNumber -> do
      mbSSN <- QDSSN.findByDriverId driverId
      return $ mapStatus <$> (mbSSN <&> (.verificationStatus))
    DVC.ProfilePhoto -> checkImageValidity DVC.ProfilePhoto driverId
    DVC.UploadProfile -> checkImageValidity DVC.UploadProfile driverId
    DVC.PanCard -> checkImageValidity DVC.PanCard driverId
    DVC.SubscriptionPlan -> do
      (autoPayStatus, mbPlan) <- DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION driverId -- fix later on basis of vehicle category
      return $ Just $ boolToStatus $ isJust mbPlan && autoPayStatus == Just DI.ACTIVE
    _ -> return Nothing
  where
    boolToStatus :: Bool -> ResponseStatus
    boolToStatus = \case
      True -> VALID
      False -> MANUAL_VERIFICATION_REQUIRED

getProcessedVehicleDocuments :: DVC.DocumentType -> Id SP.Person -> RC.VehicleRegistrationCertificate -> Flow (Maybe ResponseStatus)
getProcessedVehicleDocuments docType driverId vehicleRC =
  case docType of
    DVC.VehicleRegistrationCertificate -> return $ Just $ mapStatus vehicleRC.verificationStatus
    DVC.VehiclePermit -> do
      mbDoc <- listToMaybe <$> VPQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return $ mapStatus <$> (mbDoc <&> (.verificationStatus))
    DVC.VehicleFitnessCertificate -> do
      mbDoc <- listToMaybe <$> VFCQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return $ mapStatus <$> (mbDoc <&> (.verificationStatus))
    DVC.VehicleInsurance -> do
      mbDoc <- listToMaybe <$> VIQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return $ mapStatus <$> (mbDoc <&> (.verificationStatus))
    DVC.VehiclePUC -> do
      mbDoc <- listToMaybe <$> VPUCQuery.findByRcIdAndDriverId vehicleRC.id driverId
      return $ mapStatus <$> (mbDoc <&> (.verificationStatus))
    DVC.VehicleInspectionForm -> checkImageValidity DVC.VehicleInspectionForm driverId
    _ -> return Nothing

checkImageValidity :: DVC.DocumentType -> Id SP.Person -> Flow (Maybe ResponseStatus)
checkImageValidity docType driverId = do
  validImages <- IQuery.findImageByPersonIdAndImageTypeAndVerificationStatus driverId docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == (Just Documents.VALID)) validImages = return (Just VALID)
      | any (\img -> img.verificationStatus == (Just Documents.MANUAL_VERIFICATION_REQUIRED)) validImages = return (Just MANUAL_VERIFICATION_REQUIRED)
      | otherwise = return Nothing

getInProgressDriverDocuments :: DVC.DocumentType -> Id SP.Person -> Int -> Flow (ResponseStatus, Maybe Text)
getInProgressDriverDocuments docType driverId onboardingTryLimit =
  case docType of
    DVC.DriverLicense -> checkIfUnderProgress DVC.DriverLicense driverId onboardingTryLimit
    DVC.AadhaarCard -> checkIfImageUploaded DVC.AadhaarCard driverId
    DVC.PanCard -> checkIfImageUploaded DVC.PanCard driverId
    DVC.Permissions -> return (VALID, Nothing)
    DVC.ProfilePhoto -> checkIfImageUploaded DVC.ProfilePhoto driverId
    DVC.UploadProfile -> checkIfImageUploaded DVC.UploadProfile driverId
    _ -> return (NO_DOC_AVAILABLE, Nothing)

getInProgressVehicleDocuments :: DVC.DocumentType -> Id SP.Person -> Int -> Flow (ResponseStatus, Maybe Text)
getInProgressVehicleDocuments docType driverId onboardingTryLimit =
  case docType of
    DVC.VehicleRegistrationCertificate -> checkIfUnderProgress DVC.VehicleRegistrationCertificate driverId onboardingTryLimit
    DVC.SubscriptionPlan -> return (NO_DOC_AVAILABLE, Nothing)
    DVC.VehiclePermit -> checkIfImageUploaded DVC.VehiclePermit driverId
    DVC.VehicleFitnessCertificate -> checkIfImageUploaded DVC.VehicleFitnessCertificate driverId
    DVC.VehicleInsurance -> checkIfImageUploaded DVC.VehicleInsurance driverId
    DVC.VehiclePUC -> checkIfImageUploaded DVC.VehiclePUC driverId
    DVC.VehicleInspectionForm -> checkIfImageUploaded DVC.VehicleInspectionForm driverId
    _ -> return (NO_DOC_AVAILABLE, Nothing)

checkIfImageUploaded :: DVC.DocumentType -> Id SP.Person -> Flow (ResponseStatus, Maybe Text)
checkIfImageUploaded docType driverId = do
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
  if null images
    then return (NO_DOC_AVAILABLE, Nothing)
    else return (MANUAL_VERIFICATION_REQUIRED, Nothing)

checkIfUnderProgress :: DVC.DocumentType -> Id SP.Person -> Int -> Flow (ResponseStatus, Maybe Text)
checkIfUnderProgress docType driverId onboardingTryLimit = do
  mbVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  case mbVerificationReq of
    Just verificationReq -> do
      if verificationReq.status == "pending"
        then return (PENDING, Nothing)
        else return (FAILED, verificationReq.idfyResponse)
    Nothing -> do
      images <- IQuery.findRecentByPersonIdAndImageType driverId docType
      if length images > onboardingTryLimit
        then return (LIMIT_EXCEED, Nothing)
        else return (NO_DOC_AVAILABLE, Nothing)

documentStatusMessage :: ResponseStatus -> Maybe Text -> DVC.DocumentType -> Language -> Flow Text
documentStatusMessage status mbReason docType language = do
  case (status, docType) of
    (VALID, _) -> toVerificationMessage DocumentValid language
    (MANUAL_VERIFICATION_REQUIRED, _) -> toVerificationMessage UnderManualReview language
    (PENDING, _) -> toVerificationMessage VerificationInProgress language
    (LIMIT_EXCEED, _) -> toVerificationMessage LimitExceed language
    (NO_DOC_AVAILABLE, _) -> toVerificationMessage NoDcoumentFound language
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> toVerificationMessage RCInvalid language
    (INVALID, _) -> toVerificationMessage DocumentInvalid language
    (FAILED, _) -> do
      case mbReason of
        Just res
          | "id_not_found" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
          | "source_down" `T.isInfixOf` res -> toVerificationMessage VerificationInProgress language
          | "TIMEOUT" `T.isInfixOf` res -> toVerificationMessage VerficationFailed language
          | "BAD_REQUEST" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
          | otherwise -> toVerificationMessage Other language
        Nothing -> toVerificationMessage Other language

getAadhaarStatus :: Id SP.Person -> Flow (ResponseStatus, Maybe AV.AadhaarVerification)
getAadhaarStatus personId = do
  mAadhaarCard <- SAV.findByDriverId personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.isVerified
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Language -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverId merchantOpCityId onboardingTryLimit language = do
  mDriverLicense <- DLQuery.findByDriverId driverId
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

getRCAndStatus :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> Maybe Bool -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
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

verificationStatusCheck :: ResponseStatus -> Language -> DVC.DocumentType -> Flow Text
verificationStatusCheck status language img = do
  case (status, img) of
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> toVerificationMessage RCInvalid language
    _ -> toVerificationMessage DocumentValid language

checkIfInVerification :: Id SP.Person -> Id DMOC.MerchantOperatingCity -> Int -> DVC.DocumentType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverId _merchantOpCityId onboardingTryLimit docType language = do
  verificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType Nothing Nothing driverId docType
  images <- IQuery.findRecentByPersonIdAndImageType driverId docType
  verificationStatusWithMessage onboardingTryLimit (length images) verificationReq language

verificationStatusWithMessage :: Int -> Int -> Maybe IV.IdfyVerification -> Language -> Flow (ResponseStatus, Text)
verificationStatusWithMessage onboardingTryLimit imagesNum verificationReq language =
  case verificationReq of
    Just req -> do
      if req.status == "pending"
        then do
          msg <- toVerificationMessage VerificationInProgress language
          return (PENDING, msg)
        else do
          message <- getMessageFromResponse language req.idfyResponse
          return (FAILED, message)
    Nothing -> do
      if imagesNum > onboardingTryLimit
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
  | UnderManualReview
  | Other
  deriving (Show, Eq, Ord)

toVerificationMessage :: VerificationMessage -> Language -> Flow Text
toVerificationMessage msg lang = do
  errorTranslations <- MTQuery.findByErrorAndLanguage (T.pack (show msg)) lang
  case errorTranslations of
    Just errorTranslation -> return $ errorTranslation.message
    Nothing -> return "Something went wrong"
