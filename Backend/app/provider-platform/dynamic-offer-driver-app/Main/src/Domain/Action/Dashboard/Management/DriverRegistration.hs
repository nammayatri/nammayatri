{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.DriverRegistration
  ( getDriverRegistrationDocumentsList,
    getDriverRegistrationGetDocument,
    postDriverRegistrationDocumentUpload,
    postDriverRegistrationDocumentsCommon,
    postDriverRegistrationRegisterDl,
    postDriverRegistrationRegisterRc,
    postDriverRegistrationRegisterGenerateAadhaarOtp,
    postDriverRegistrationRegisterVerifyAadhaarOtp,
    getDriverRegistrationUnderReviewDrivers,
    getDriverRegistrationDocumentsInfo,
    postDriverRegistrationDocumentsUpdate,
    postDriverRegistrationRegisterAadhaar,
    postDriverRegistrationUnlinkDocument,
    getDriverRegistrationVerificationStatus,
  )
where

import qualified API.Types.ProviderPlatform.Management.Account as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import qualified API.Types.UI.DriverOnboardingV2
import qualified Data.HashMap.Strict as HM
import qualified Data.Tuple.Extra as TE
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Dashboard.Management.Driver as DDriver
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Types.BusinessLicense as DBL
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCommonDoc
import qualified Domain.Types.DocumentVerificationConfig as Domain
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehicleNOC as DNOC
import qualified Domain.Types.VehiclePUC as DPUC
import qualified Domain.Types.VehiclePermit as DVPermit
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Environment
import EulerHS.Prelude hiding (elem, find, foldl', map, whenJust)
import Kernel.Beam.Functions
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.Encryption (decrypt, encrypt, hash)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Documents (VerificationStatus (..))
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BusinessLicense as QBL
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDriverOnboardingDocuments
import qualified Storage.Queries.DriverGstin as QGstin
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QPan
import qualified Storage.Queries.DriverSSN as QSSN
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as QFODVC
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.HyperVergeVerificationExtra as HVQuery
import qualified Storage.Queries.IdfyVerificationExtra as IDQuery
import Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QDriver
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehicleNOC as QVNOC
import qualified Storage.Queries.VehiclePUC as QVPUC
import qualified Storage.Queries.VehiclePermit as QVPermit
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error
import Tools.Notifications as Notify

getDriverRegistrationDocumentsList :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Maybe Text -> Flow Common.DocumentsListResponse
getDriverRegistrationDocumentsList merchantShortId city driverId mbRcId = do
  merchant <- findMerchantByShortId merchantShortId
  odometerImg <- getVehicleImages merchant.id Domain.Odometer
  vehicleFrontImgs <- getVehicleImages merchant.id Domain.VehicleFront
  vehicleBackImgs <- getVehicleImages merchant.id Domain.VehicleBack
  vehicleRightImgs <- getVehicleImages merchant.id Domain.VehicleRight
  vehicleLeftImgs <- getVehicleImages merchant.id Domain.VehicleLeft
  vehicleFrontInteriorImgs <- getVehicleImages merchant.id Domain.VehicleFrontInterior
  vehicleBackInteriorImgs <- getVehicleImages merchant.id Domain.VehicleBackInterior
  pucImages <- getDriverImages merchant.id Domain.VehiclePUC
  permitImages <- getDriverImages merchant.id Domain.VehiclePermit
  dlImgs <- groupByTxnIdInHM <$> runInReplica (findImagesByPersonAndType Nothing Nothing merchant.id (cast driverId) Domain.DriverLicense)
  vInspectionImgs <- getDriverImages merchant.id Domain.VehicleInspectionForm
  vehRegImgs <- getDriverImages merchant.id Domain.VehicleRegistrationCertificate
  uploadProfImgs <- getDriverImages merchant.id Domain.UploadProfile
  vehicleFitnessCertImgs <- getDriverImages merchant.id Domain.VehicleFitnessCertificate
  vehicleInsImgs <- getDriverImages merchant.id Domain.VehicleInsurance
  profilePics <- getDriverImages merchant.id Domain.ProfilePhoto
  gstImgs <- getDriverImages merchant.id Domain.GSTCertificate
  panImgs <- getDriverImages merchant.id Domain.PanCard
  businessLicenseImgs <- getDriverImages merchant.id Domain.BusinessLicense
  aadhaarImgs <- getDriverImages merchant.id Domain.AadhaarCard
  vehicleNOCImgs <- getDriverImages merchant.id Domain.VehicleNOC
  commonDocumentsData <- runInReplica (QCommonDriverOnboardingDocuments.findByDriverId (Just (cast driverId)))
  let commonDocuments = map toCommonDocumentItem commonDocumentsData
  allDlImgs <- runInReplica (QDL.findAllByImageId (map (Id) $ mapMaybe listToMaybe dlImgs))
  allRCImgs <- runInReplica (QRC.findAllByImageId (map (Id) vehRegImgs))
  allDLDetails <- mapM convertDLToDLDetails allDlImgs
  allRCDetails <- mapM convertRCToRCDetails allRCImgs
  ssnEntry <- QSSN.findByDriverId (cast driverId)
  ssnUnenc <- case ssnEntry of
    Just ssnRecord -> do
      ssnDecrypt <- decrypt ssnRecord.ssn
      return $ Just ssnDecrypt
    _ -> return Nothing
  pure
    Common.DocumentsListResponse
      { driverLicense = dlImgs,
        vehicleRegistrationCertificate = vehRegImgs,
        vehicleInsurance = vehicleInsImgs,
        uploadProfile = uploadProfImgs,
        ssn = ssnUnenc,
        vehicleFitnessCertificate = vehicleFitnessCertImgs,
        profilePhoto = profilePics,
        driverLicenseDetails = allDLDetails,
        vehicleRegistrationCertificateDetails = allRCDetails,
        vehicleInspectionForm = vInspectionImgs,
        vehiclePermit = permitImages,
        vehiclePUC = pucImages,
        vehicleFront = vehicleFrontImgs,
        vehicleBack = vehicleBackImgs,
        vehicleRight = vehicleRightImgs,
        vehicleLeft = vehicleLeftImgs,
        vehicleFrontInterior = vehicleFrontInteriorImgs,
        vehicleBackInterior = vehicleBackInteriorImgs,
        pan = panImgs,
        businessLicense = businessLicenseImgs,
        aadhaar = aadhaarImgs,
        vehicleNOC = vehicleNOCImgs,
        odometer = odometerImg,
        gstCertificate = gstImgs,
        commonDocuments = commonDocuments
      }
  where
    getVehicleImages merchantId imageType = case mbRcId of
      Just rcId -> map (.id.getId) <$> runInReplica (findImagesByRCAndType merchantId (Just rcId) imageType Nothing)
      Nothing -> pure []

    getDriverImages merchantId imageType = map (.id.getId) <$> runInReplica (findImagesByPersonAndType Nothing Nothing merchantId (cast driverId) imageType)

    groupByTxnIdInHM = handleNullTxnIds . foldl' (\acc img -> HM.insertWith (++) (fromMaybe "Nothing" img.workflowTransactionId) [img.id.getId] acc) (HM.empty :: HM.HashMap Text [Text])
    handleNullTxnIds hm = (maybe [] (map (: [])) $ HM.lookup "Nothing" hm) ++ (HM.elems $ HM.delete "Nothing" hm)
    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      pure $
        Common.DLDetails
          { driverName = dl.driverName,
            driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show city,
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
        Common.RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show city,
            vehicleCategory = show <$> rc.userPassedVehicleCategory,
            airConditioned = rc.airConditioned,
            oxygen = rc.oxygen,
            ventilator = rc.ventilator,
            vehicleManufacturer = rc.vehicleManufacturer,
            vehicleModel = rc.vehicleModel,
            vehicleColor = rc.vehicleColor,
            vehicleDoors = rc.vehicleDoors,
            vehicleSeatBelts = rc.vehicleSeatBelts,
            createdAt = rc.createdAt,
            dateOfRegistration = rc.dateOfRegistration,
            vehicleModelYear = rc.vehicleModelYear,
            failedRules = rc.failedRules
          }

    toCommonDocumentItem :: DCommonDoc.CommonDriverOnboardingDocuments -> Common.CommonDocumentItem
    toCommonDocumentItem doc =
      Common.CommonDocumentItem
        { documentId = cast doc.id,
          documentType = SDO.castDocumentType doc.documentType,
          documentData = doc.documentData,
          verificationStatus = DCommon.castVerificationStatus doc.verificationStatus,
          rejectReason = doc.rejectReason,
          documentImageId = getId <$> doc.documentImageId,
          createdAt = doc.createdAt,
          updatedAt = doc.updatedAt
        }

getDriverRegistrationGetDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Image -> Flow Common.GetDocumentResponse
getDriverRegistrationGetDocument merchantShortId _ imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  image <- QImage.findById (cast imageId) >>= fromMaybeM (InternalError "Image not found by image id")
  pure Common.GetDocumentResponse {imageBase64 = img, status = castVerificationStatus <$> image.verificationStatus}
  where
    castVerificationStatus :: VerificationStatus -> Common.VerificationStatus
    castVerificationStatus = \case
      PENDING -> Common.PENDING
      VALID -> Common.VALID
      INVALID -> Common.INVALID
      MANUAL_VERIFICATION_REQUIRED -> Common.MANUAL_VERIFICATION_REQUIRED
      UNAUTHORIZED -> Common.UNAUTHORIZED

mapDocumentType :: Common.DocumentType -> Domain.DocumentType
mapDocumentType Common.DriverLicense = Domain.DriverLicense
mapDocumentType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate
mapDocumentType Common.VehiclePUCImage = Domain.VehiclePUC
mapDocumentType Common.VehiclePermitImage = Domain.VehiclePermit
mapDocumentType Common.VehicleInsuranceImage = Domain.VehicleInsurance
mapDocumentType Common.VehicleFitnessCertificateImage = Domain.VehicleFitnessCertificate
mapDocumentType Common.VehicleInspectionImage = Domain.VehicleInspectionForm
mapDocumentType Common.ProfilePhotoImage = Domain.ProfilePhoto
mapDocumentType Common.PanCard = Domain.PanCard
mapDocumentType Common.Permissions = Domain.Permissions
mapDocumentType Common.SubscriptionPlan = Domain.SubscriptionPlan
mapDocumentType Common.ProfileDetails = Domain.ProfileDetails
mapDocumentType Common.AadhaarCard = Domain.AadhaarCard
mapDocumentType Common.SocialSecurityNumber = Domain.SocialSecurityNumber
mapDocumentType Common.GSTCertificate = Domain.GSTCertificate
mapDocumentType Common.BackgroundVerification = Domain.BackgroundVerification
mapDocumentType Common.UploadProfileImage = Domain.UploadProfile
mapDocumentType Common.VehicleNOC = Domain.VehicleNOC
mapDocumentType Common.BusinessLicense = Domain.BusinessLicense
mapDocumentType Common.VehicleFront = Domain.VehicleFront
mapDocumentType Common.VehicleBack = Domain.VehicleBack
mapDocumentType Common.VehicleFrontInterior = Domain.VehicleFrontInterior
mapDocumentType Common.VehicleBackInterior = Domain.VehicleBackInterior
mapDocumentType Common.VehicleLeft = Domain.VehicleLeft
mapDocumentType Common.VehicleRight = Domain.VehicleRight
mapDocumentType Common.Odometer = Domain.Odometer
mapDocumentType Common.InspectionHub = Domain.InspectionHub
-- Netherlands Document Types
mapDocumentType Common.KIWADriverCard = Domain.KIWADriverCard
mapDocumentType Common.KIWATaxiPermit = Domain.KIWATaxiPermit
mapDocumentType Common.KvKChamberOfCommerceRegistration = Domain.KvKChamberOfCommerceRegistration
mapDocumentType Common.TAXDetails = Domain.TAXDetails
mapDocumentType Common.BankingDetails = Domain.BankingDetails
mapDocumentType Common.VehicleDetails = Domain.VehicleDetails
mapDocumentType Common.SchipolAirportAgreement = Domain.SchipolAirportAgreement
mapDocumentType Common.SchipolSmartcardProof = Domain.SchipolSmartcardProof
mapDocumentType Common.TXQualityMark = Domain.TXQualityMark
-- Finland Document Types
mapDocumentType Common.TaxiDriverPermit = Domain.TaxiDriverPermit
mapDocumentType Common.TaxiTransportLicense = Domain.TaxiTransportLicense
mapDocumentType Common.FinnishIDResidencePermit = Domain.FinnishIDResidencePermit
mapDocumentType Common.BusinessRegistrationExtract = Domain.BusinessRegistrationExtract

postDriverRegistrationDocumentUpload :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
postDriverRegistrationDocumentUpload merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  whenJust req.requestorId $ \requestorId -> do
    entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, cast driverId_] merchantOpCityId
    entity <- find (\e -> e.id == cast driverId_) entities & fromMaybeM (PersonDoesNotExist driverId_.getId)
    requestor <- find (\e -> e.id == Id requestorId) entities & fromMaybeM (PersonDoesNotExist requestorId)
    isValid <- DDriver.isAssociationBetweenTwoPerson requestor entity
    unless isValid $ throwError (InvalidRequest "Driver is not associated with the entity")
  res <-
    validateImage
      True
      (cast driverId_, cast merchant.id, merchantOpCityId)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapDocumentType req.imageType,
          rcNumber = req.rcNumber,
          validationStatus = Nothing,
          workflowTransactionId = Nothing,
          vehicleCategory = Nothing,
          sdkFailureReason = Nothing
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

postDriverRegistrationDocumentsCommon ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.CommonDocumentCreateReq ->
  Flow APISuccess
postDriverRegistrationDocumentsCommon merchantShortId opCity driverId Common.CommonDocumentCreateReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let driverPersonId = cast @Common.Driver @DP.Person driverId
  void $ QPerson.findById driverPersonId >>= fromMaybeM (PersonNotFound driverPersonId.getId)
  whenJust imageId $ \imgId -> do
    void $ QImage.findById (cast imgId) >>= fromMaybeM (InvalidRequest "Image not found")
  documentId <- generateGUID
  now <- getCurrentTime
  let documentEntry =
        DCommonDoc.CommonDriverOnboardingDocuments
          { id = documentId,
            documentImageId = cast <$> imageId,
            documentType = mapDocumentType documentType,
            driverId = Just driverPersonId,
            documentData = documentData,
            rejectReason = Nothing,
            verificationStatus = Documents.MANUAL_VERIFICATION_REQUIRED,
            merchantOperatingCityId = merchantOpCityId,
            merchantId = merchant.id,
            createdAt = now,
            updatedAt = now
          }
  QCommonDriverOnboardingDocuments.create documentEntry
  pure Success

postDriverRegistrationUnlinkDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.DocumentType -> Maybe Text -> Flow Common.UnlinkDocumentResp
postDriverRegistrationUnlinkDocument merchantShortId opCity personId documentType mbRequestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  person <- case mbRequestorId of
    Just requestorId -> do
      entities <- QPerson.findAllByPersonIdsAndMerchantOpsCityId [Id requestorId, cast personId] merchantOpCityId
      entity <- find (\e -> e.id == cast personId) entities & fromMaybeM (PersonDoesNotExist personId.getId)
      requestor <- find (\e -> e.id == Id requestorId) entities & fromMaybeM (PersonDoesNotExist requestorId)
      isValid <- DDriver.isAssociationBetweenTwoPerson requestor entity
      unless isValid $ throwError (InvalidRequest "Driver is not associated with the entity")
      pure entity
    Nothing -> runInReplica $ QPerson.findById (cast personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
  res <- unlinkPersonDocument merchantOpCityId person
  return
    Common.UnlinkDocumentResp
      { mandatoryDocumentRemoved = res
      }
  where
    unlinkPersonDocument :: Id DMOC.MerchantOperatingCity -> DP.Person -> Flow Bool
    unlinkPersonDocument merchantOpCityId person = do
      case person.role of
        DP.FLEET_OWNER -> do
          case documentType of
            Common.PanCard -> QFOI.updatePanImage Nothing Nothing person.id
            Common.GSTCertificate -> QFOI.updateGstImage Nothing Nothing person.id
            Common.AadhaarCard -> QFOI.updateAadhaarImage Nothing Nothing Nothing person.id
            _ -> throwError (InvalidRequest "Invalid document type")
        DP.DRIVER -> do
          case documentType of
            Common.PanCard -> QDriverInfo.updatePanNumber Nothing person.id
            Common.AadhaarCard -> QDriverInfo.updateAadhaarNumber Nothing person.id
            _ -> throwError (InvalidRequest "Invalid document type")
        _ -> pure ()
      case documentType of
        Common.PanCard -> QPan.deleteByDriverId person.id
        Common.GSTCertificate -> QGstin.deleteByDriverId person.id
        Common.AadhaarCard -> QAadhaarCard.deleteByPersonId person.id
        Common.DriverLicense -> QDL.deleteByDriverId person.id
        _ -> throwError (InvalidRequest "Invalid document type")
      checkAndUpdateEnabledStatus merchantOpCityId documentType person

    checkAndUpdateEnabledStatus :: Id DMOC.MerchantOperatingCity -> Common.DocumentType -> DP.Person -> Flow Bool
    checkAndUpdateEnabledStatus merchantOpCityId docType person = do
      case person.role of
        role | DCommon.checkFleetOwnerRole role -> do
          mbFleetVerificationConfig <- QFODVC.findByPrimaryKey (mapDocumentType docType) merchantOpCityId person.role
          let isMandatory = maybe False (.isMandatory) mbFleetVerificationConfig
          if isMandatory
            then do
              QFOI.updateFleetOwnerEnabledStatus False person.id
              pure True
            else pure False
        DP.DRIVER -> do
          mbVerificationConfig <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId (mapDocumentType docType) DVC.CAR Nothing
          let isMandatory = maybe False (\config -> fromMaybe config.isMandatory config.isMandatoryForEnabling) mbVerificationConfig
          when isMandatory $ do
            transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
            Analytics.updateEnabledVerifiedStateWithAnalytics Nothing transporterConfig person.id False Nothing
          pure False
        _ -> pure False

postDriverRegistrationRegisterDl :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
postDriverRegistrationRegisterDl merchantShortId opCity driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let verifyBy = case accessType of
        Just accessTypeValue -> case accessTypeValue of
          Common.DASHBOARD_ADMIN -> DPan.DASHBOARD_ADMIN
          Common.DASHBOARD_USER -> DPan.DASHBOARD_USER
          _ -> DPan.DASHBOARD
        Nothing -> DPan.DASHBOARD
  verifyDL
    verifyBy
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        vehicleCategory = Nothing,
        nameOnCardFromSdk = Nothing,
        requestId = Nothing,
        sdkTransactionId = Nothing,
        ..
      }

postDriverRegistrationRegisterRc :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
postDriverRegistrationRegisterRc merchantShortId opCity driverId_ Common.RegisterRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  verifyRC
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    ( DriverRCReq
        { imageId = cast imageId,
          vehicleCategory = Nothing,
          vehicleDetails = Nothing,
          ..
        }
    )
    False
    Nothing

postDriverRegistrationRegisterAadhaar :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.AadhaarCardReq -> Flow APISuccess
postDriverRegistrationRegisterAadhaar merchantShortId opCity driverId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  DOV.postDriverRegisterAadhaarCard (Just (cast driverId), merchant.id, merchantOpCityId) (castAadharReq req)
  where
    castAadharReq Common.AadhaarCardReq {..} = API.Types.UI.DriverOnboardingV2.AadhaarCardReq {aadhaarBackImageId = cast <$> aadhaarBackImageId, aadhaarFrontImageId = cast <$> aadhaarFrontImageId, validationStatus = convertValidationStatus validationStatus, ..}
    convertValidationStatus status = case status of
      Common.APPROVED -> API.Types.UI.DriverOnboardingV2.APPROVED
      Common.DECLINED -> API.Types.UI.DriverOnboardingV2.DECLINED
      Common.AUTO_APPROVED -> API.Types.UI.DriverOnboardingV2.AUTO_APPROVED
      Common.AUTO_DECLINED -> API.Types.UI.DriverOnboardingV2.AUTO_DECLINED
      Common.NEEDS_REVIEW -> API.Types.UI.DriverOnboardingV2.NEEDS_REVIEW

--make a separate function casting the driverVehiclereq

postDriverRegistrationRegisterGenerateAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
postDriverRegistrationRegisterGenerateAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.generateAadhaarOtp
      True
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AadhaarVerification.AadhaarOtpReq
        { aadhaarNumber = req.aadhaarNumber,
          consent = req.consent
        }
  pure (convertVerifyOtp res)

postDriverRegistrationRegisterVerifyAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
postDriverRegistrationRegisterVerifyAadhaarOtp merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    AV.verifyAadhaarOtp
      (Just merchant)
      (cast driverId_)
      merchantOpCityId
      AV.VerifyAadhaarOtpReq
        { otp = req.otp,
          shareCode = req.shareCode
        }
  pure (convertSubmitOtp res)

getDriverRegistrationUnderReviewDrivers :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
getDriverRegistrationUnderReviewDrivers _merchantShortId _opCity _limit _offset = throwError (InternalError "Not Implemented")

getDriverRegistrationDocumentsInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow [Common.DriverDocument]
getDriverRegistrationDocumentsInfo _merchantShortId _opCity _driverId = throwError (InternalError "Not Implemented")

approveAndUpdateRC :: Common.RCApproveDetails -> Flow ()
approveAndUpdateRC req = do
  let imageId = Id req.documentImageId.getId
  rc <- QRC.findByImageId imageId >>= fromMaybeM (InternalError "RC not found by image id")
  certificateNumber <- mapM encrypt req.vehicleNumberPlate
  let udpatedRC =
        rc
          { DRC.vehicleVariant = req.vehicleVariant <|> rc.vehicleVariant,
            DRC.verificationStatus = VALID,
            DRC.certificateNumber = fromMaybe rc.certificateNumber certificateNumber,
            DRC.vehicleManufacturer = req.vehicleManufacturer <|> rc.vehicleManufacturer,
            DRC.vehicleModel = req.vehicleModel <|> rc.vehicleModel,
            DRC.vehicleModelYear = req.vehicleModelYear <|> rc.vehicleModelYear,
            DRC.vehicleColor = req.vehicleColor <|> rc.vehicleColor,
            DRC.vehicleDoors = req.vehicleDoors <|> rc.vehicleDoors,
            DRC.vehicleSeatBelts = req.vehicleSeatBelts <|> rc.vehicleSeatBelts,
            DRC.fitnessExpiry = if isJust req.fitnessExpiry then fromJust req.fitnessExpiry else rc.fitnessExpiry,
            DRC.permitExpiry = req.permitExpiry <|> rc.permitExpiry
          }
  QRC.updateByPrimaryKey udpatedRC
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.VehicleRegistrationCertificate

approveAndUpdateInsurance :: Common.VInsuranceApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateInsurance req@Common.VInsuranceApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) req.policyExpiry Domain.VehicleInsurance imageId
  vinsurance <- QVI.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  case vinsurance of
    Just insurance -> do
      policyNo <- mapM encrypt req.policyNumber
      let updatedInsurance =
            insurance
              { DVI.insuredName = req.insuredName <|> insurance.insuredName,
                DVI.issueDate = req.issueDate <|> insurance.issueDate,
                DVI.limitsOfLiability = req.limitsOfLiability <|> insurance.limitsOfLiability,
                DVI.policyNumber = fromMaybe insurance.policyNumber policyNo,
                DVI.policyProvider = fromMaybe insurance.policyProvider req.policyProvider,
                DVI.verificationStatus = VALID,
                DVI.policyExpiry = fromMaybe insurance.policyExpiry req.policyExpiry
              }
      QVI.updateByPrimaryKey updatedInsurance
    Nothing -> do
      case (req.policyNumber, req.policyExpiry, req.policyProvider, req.rcNumber) of
        (Just policyNum, Just policyExp, Just provider, Just rcNo) -> do
          insuranceImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
          rcNoEnc <- encrypt rcNo
          rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
          policyNo <- encrypt policyNum
          let insurance =
                DVI.VehicleInsurance
                  { documentImageId = imageId,
                    driverId = insuranceImage.personId,
                    id = uuid,
                    policyNumber = policyNo,
                    rcId = rc.id,
                    verificationStatus = VALID,
                    createdAt = now,
                    updatedAt = now,
                    merchantId = Just mId,
                    merchantOperatingCityId = Just mOpCityId,
                    policyExpiry = policyExp,
                    policyProvider = provider,
                    rejectReason = Nothing,
                    ..
                  }
          QVI.create insurance
        _ -> do
          transporterConfig <- CCT.findByMerchantOpCityId mOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound mOpCityId.getId)
          case transporterConfig.createDocumentRequired of
            Just True -> throwError (InternalError "Provide all the details for creating insurance document: policyNumber, policyExpiry, policyProvider, rcNumber")
            _ -> pure ()

approveAndUpdatePUC :: Common.VPUCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePUC req@Common.VPUCApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.pucExpiry) Domain.VehiclePUC imageId
  vpuc <- QVPUC.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  pucNoEnc <- encrypt req.pucNumber
  case vpuc of
    Just puc -> do
      let updatedpuc =
            puc{DPUC.pucNumber = Just pucNoEnc,
                DPUC.pucExpiry = req.pucExpiry,
                DPUC.rcId = rc.id,
                DPUC.testDate = req.testDate <|> puc.testDate,
                DPUC.verificationStatus = VALID
               }
      QVPUC.updateByPrimaryKey updatedpuc
    Nothing -> do
      pucImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      let puc =
            DPUC.VehiclePUC
              { documentImageId = imageId,
                driverId = pucImage.personId,
                id = uuid,
                pucExpiry = req.pucExpiry,
                pucNumber = Just pucNoEnc,
                rcId = rc.id,
                testDate = req.testDate,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVPUC.create puc

approveAndUpdatePermit :: Common.VPermitApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePermit req@Common.VPermitApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.permitExpiry) Domain.VehiclePermit imageId
  vPremit <- QVPermit.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  permitNumEnc <- encrypt req.permitNumber
  case vPremit of
    Just permit -> do
      let updatedpermit =
            permit
              { DVPermit.issueDate = req.issueDate <|> permit.issueDate,
                DVPermit.nameOfPermitHolder = req.nameOfPermitHolder <|> permit.nameOfPermitHolder,
                DVPermit.permitExpiry = req.permitExpiry,
                DVPermit.permitNumber = permitNumEnc,
                DVPermit.purposeOfJourney = req.purposeOfJourney <|> permit.purposeOfJourney,
                DVPermit.rcId = rc.id,
                DVPermit.regionCovered = req.regionCovered,
                DVPermit.verificationStatus = VALID
              }
      QVPermit.updateByPrimaryKey updatedpermit
    Nothing -> do
      permitImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      let permit =
            DVPermit.VehiclePermit
              { documentImageId = imageId,
                driverId = permitImage.personId,
                id = uuid,
                issueDate = req.issueDate,
                nameOfPermitHolder = req.nameOfPermitHolder,
                permitExpiry = req.permitExpiry,
                permitNumber = permitNumEnc,
                purposeOfJourney = req.purposeOfJourney,
                rcId = rc.id,
                regionCovered = req.regionCovered,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVPermit.create permit

approveAndUpdateFitnessCertificate :: Common.FitnessApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateFitnessCertificate req@Common.FitnessApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusByIdAndType VALID imageId Domain.VehicleFitnessCertificate
  mbFitnessCert <- QFC.findByImageId imageId
  applicationNo <- encrypt req.applicationNumber
  now <- getCurrentTime
  uuid <- generateGUID
  case mbFitnessCert of
    Just certificate -> do
      let updatedFitnessCert =
            certificate
              { DFC.applicationNumber = applicationNo,
                DFC.categoryOfVehicle = req.categoryOfVehicle <|> certificate.categoryOfVehicle,
                DFC.fitnessExpiry = req.fitnessExpiry,
                DFC.inspectingAuthority = req.inspectingAuthority <|> certificate.inspectingAuthority,
                DFC.inspectingOn = req.inspectingOn <|> certificate.inspectingOn,
                DFC.nextInspectionDate = req.nextInspectionDate <|> certificate.nextInspectionDate,
                DFC.receiptDate = req.receiptDate <|> certificate.receiptDate,
                DFC.verificationStatus = VALID
              }
      QFC.updateByPrimaryKey updatedFitnessCert
    Nothing -> do
      certificateImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      rcNoEnc <- encrypt rcNumber
      rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by rc number")
      let fitnessCert =
            DFC.VehicleFitnessCertificate
              { applicationNumber = applicationNo,
                documentImageId = imageId,
                driverId = certificateImage.personId,
                id = uuid,
                rcId = rc.id,
                verificationStatus = VALID,
                createdAt = now,
                updatedAt = now,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                ..
              }
      QFC.create fitnessCert

approveAndUpdateDL :: Common.DLApproveDetails -> Flow ()
approveAndUpdateDL req = do
  let imageId = Id req.documentImageId.getId
  dl <- QDL.findByImageId imageId >>= fromMaybeM (InternalError "DL not found by image id")
  licenseNumber <- mapM encrypt req.driverLicenseNumber
  let updatedDL =
        dl
          { DDL.licenseNumber = fromMaybe dl.licenseNumber licenseNumber,
            DDL.driverDob = req.driverDateOfBirth <|> dl.driverDob,
            DDL.licenseExpiry = fromMaybe dl.licenseExpiry req.dateOfExpiry,
            DDL.verificationStatus = VALID
          }
  QDL.updateByPrimaryKey updatedDL
  void $ uncurry (liftA2 (,)) $ TE.both (maybe (return ()) (flip (QImage.updateVerificationStatusByIdAndType VALID) Domain.DriverLicense)) (Just dl.documentImageId1, dl.documentImageId2)

approveAndUpdateNOC :: Common.NOCApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateNOC req@Common.NOCApproveDetails {..} mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.nocExpiry) Domain.VehicleNOC imageId
  vnoc <- QVNOC.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  rcNoEnc <- encrypt rcNumber
  rc <- QRC.findByCertificateNumberHash (rcNoEnc & hash) >>= fromMaybeM (InternalError "RC not found by RC number")
  nocNoEnc <- encrypt req.nocNumber
  case vnoc of
    Just noc -> do
      let updatednoc =
            noc{DNOC.nocNumber = nocNoEnc,
                DNOC.nocExpiry = req.nocExpiry,
                DNOC.rcId = rc.id,
                DNOC.verificationStatus = VALID
               }
      QVNOC.updateByPrimaryKey updatednoc
    Nothing -> do
      nocImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      let noc =
            DNOC.VehicleNOC
              { documentImageId = imageId,
                driverId = nocImage.personId,
                id = uuid,
                nocExpiry = req.nocExpiry,
                nocNumber = nocNoEnc,
                rcId = rc.id,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QVNOC.create noc

approveAndUpdateBusinessLicense :: Common.BusinessLicenseApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateBusinessLicense req mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusAndExpiry (Just VALID) (Just req.licenseExpiry) Domain.BusinessLicense imageId
  mbBl <- QBL.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  businessLicenseNumberEnc <- encrypt req.businessLicenseNumber
  case mbBl of
    Just bl -> do
      let updatedBl =
            bl{DBL.licenseNumber = businessLicenseNumberEnc,
               DBL.licenseExpiry = req.licenseExpiry,
               DBL.verificationStatus = VALID
              }
      QBL.updateByPrimaryKey updatedBl
    Nothing -> do
      blImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      let bl =
            DBL.BusinessLicense
              { documentImageId = imageId,
                driverId = blImage.personId,
                id = uuid,
                licenseExpiry = req.licenseExpiry,
                licenseNumber = businessLicenseNumberEnc,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now
              }
      QBL.create bl

approveAndUpdatePan :: Common.PanApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdatePan req mId mOpCityId = do
  let imageId = Id req.documentImageId.getId
  QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) Domain.PanCard
  mbPan <- QPan.findByImageId imageId
  now <- getCurrentTime
  uuid <- generateGUID
  panNoEnc <- encrypt req.panNumber
  case mbPan of
    Just pan -> do
      let updatedPan =
            pan{DPan.panCardNumber = panNoEnc,
                DPan.verificationStatus = VALID,
                docType = castReqTypeToDomain <$> req.docType,
                driverNameOnGovtDB = req.driverNameOnGovtDB,
                driverDob = req.driverDob
               }
      QPan.updateByPrimaryKey updatedPan
    Nothing -> do
      panImage <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      person <- QPerson.findById panImage.personId >>= fromMaybeM (PersonNotFound panImage.personId.getId)
      let pan =
            DPan.DriverPanCard
              { panCardNumber = panNoEnc,
                documentImageId1 = imageId,
                driverId = panImage.personId,
                id = uuid,
                verificationStatus = VALID,
                merchantId = Just mId,
                merchantOperatingCityId = Just mOpCityId,
                createdAt = now,
                updatedAt = now,
                consent = True,
                consentTimestamp = now,
                docType = castReqTypeToDomain <$> req.docType,
                documentImageId2 = Nothing,
                driverDob = req.driverDob,
                driverName = Just person.firstName,
                driverNameOnGovtDB = req.driverNameOnGovtDB,
                failedRules = [],
                verifiedBy = Just DPan.DASHBOARD
              }
      QPan.create pan

approveAndUpdateCommonDocument :: Common.CommonDocumentApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
approveAndUpdateCommonDocument req _mId _mOpCityId = do
  let documentId = Id req.documentId.getId
  -- Get the existing document
  mbDocument <- QCommonDriverOnboardingDocuments.findById documentId
  document <- mbDocument & fromMaybeM (DocumentNotFound documentId.getId)

  -- Update document with new data if provided, otherwise just update verification status
  let updatedDocument = case req.updatedDocumentData of
        Just newData -> document {DCommonDoc.documentData = newData, DCommonDoc.verificationStatus = VALID}
        Nothing -> document {DCommonDoc.verificationStatus = VALID}

  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument

rejectAndUpdateCommonDocument :: Common.CommonDocumentRejectDetails -> Id DMOC.MerchantOperatingCity -> Flow ()
rejectAndUpdateCommonDocument req _mOpCityId = do
  let documentId = Id req.documentId.getId
  -- Get the existing document
  mbDocument <- QCommonDriverOnboardingDocuments.findById documentId
  document <- mbDocument & fromMaybeM (DocumentNotFound documentId.getId)

  -- Update document with rejection status and reason
  let updatedDocument = document {DCommonDoc.verificationStatus = INVALID, DCommonDoc.rejectReason = Just req.reason}

  QCommonDriverOnboardingDocuments.updateByPrimaryKey updatedDocument

castReqTypeToDomain :: Common.PanType -> DPan.PanType
castReqTypeToDomain = \case
  Common.INDIVIDUAL -> DPan.INDIVIDUAL
  Common.BUSINESS -> DPan.BUSINESS

handleApproveRequest :: Common.ApproveDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleApproveRequest approveReq merchantId merchantOperatingCityId = do
  case approveReq of
    Common.DL dlReq -> approveAndUpdateDL dlReq
    Common.RC rcApproveReq -> approveAndUpdateRC rcApproveReq
    Common.VehicleInsurance vInsuranceReq -> approveAndUpdateInsurance vInsuranceReq merchantId merchantOperatingCityId
    Common.VehiclePUC pucReq -> approveAndUpdatePUC pucReq merchantId merchantOperatingCityId
    Common.VehiclePermit permitReq -> approveAndUpdatePermit permitReq merchantId merchantOperatingCityId
    Common.VehicleFitnessCertificate fitnessReq -> approveAndUpdateFitnessCertificate fitnessReq merchantId merchantOperatingCityId
    Common.UploadProfile imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) Domain.UploadProfile
    Common.ProfilePhoto imageId -> QImage.updateVerificationStatusByIdAndType VALID (Id imageId.getId) Domain.ProfilePhoto
    Common.VehicleInspectionForm req -> do
      QImage.updateVerificationStatusByIdAndType VALID (Id req.documentImageId.getId) Domain.VehicleInspectionForm
      QImage.updateDocumentExpiry req.dateOfExpiry (Id req.documentImageId.getId)
    Common.SSNApprove ssnNum -> do
      ssnEnc <- encrypt ssnNum
      QSSN.updateVerificationStatusAndReasonBySSN VALID Nothing (ssnEnc & hash)
    Common.NOC req -> approveAndUpdateNOC req merchantId merchantOperatingCityId
    Common.BusinessLicenseImg req -> approveAndUpdateBusinessLicense req merchantId merchantOperatingCityId
    Common.Pan req -> approveAndUpdatePan req merchantId merchantOperatingCityId
    Common.CommonDocument req -> approveAndUpdateCommonDocument req merchantId merchantOperatingCityId

handleRejectRequest :: Common.RejectDetails -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
handleRejectRequest rejectReq _ merchantOperatingCityId = do
  case rejectReq of
    Common.SSNReject ssnRejectReq -> rejectSSNAndSendNotification ssnRejectReq merchantOperatingCityId
    Common.ImageDocuments imageRejectReq -> do
      let imageId = Id imageRejectReq.documentImageId.getId
      image <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
      case image.imageType of
        Domain.ProfilePhoto -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.UploadProfile -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehicleInspectionForm -> QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehicleFitnessCertificate -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QFC.updateVerificationStatus INVALID imageId
        Domain.VehicleInsurance -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          vInsurance <- QVI.findByImageId imageId
          whenJust vInsurance $ \_ -> do
            QVI.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
        Domain.DriverLicense -> do
          dl <- QDL.findByImageId imageId >>= fromMaybeM (InternalError "DL not found by image id")
          QDL.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
          void $ uncurry (liftA2 (,)) $ TE.both (maybe (return ()) (QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason))) (Just dl.documentImageId1, dl.documentImageId2)
        Domain.VehicleRegistrationCertificate -> do
          QRC.updateVerificationStatusAndRejectReason INVALID imageRejectReq.reason imageId
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
        Domain.VehiclePermit -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QVPermit.updateVerificationStatusByImageId INVALID imageId
        Domain.VehiclePUC -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QVPUC.updateVerificationStatusByImageId INVALID imageId
        Domain.VehicleNOC -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QVNOC.updateVerificationStatusByImageId INVALID imageId
        Domain.BusinessLicense -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QBL.updateVerificationStatusByImageId INVALID imageId
        Domain.PanCard -> do
          QImage.updateVerificationStatusAndFailureReason INVALID (ImageNotValid imageRejectReq.reason) imageId
          QPan.updateVerificationStatusByImageId INVALID imageId
        _ -> throwError (InternalError "Unknown Config in reject update document")
      driver <- QDriver.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
      Notify.notifyDriver merchantOperatingCityId notificationType (notificationTitle (show image.imageType)) (message (show image.imageType)) driver driver.deviceToken
    Common.CommonDocumentReject commonRejectReq -> rejectAndUpdateCommonDocument commonRejectReq merchantOperatingCityId
  where
    notificationType = FCM.DOCUMENT_INVALID
    notificationTitle obj = "Attention: Your " <> obj <> " is invalid."
    message obj = "Kindly reapply or reupload your " <> obj
    rejectSSNAndSendNotification req merchantOpCityId = do
      ssnEnc <- encrypt req.ssn
      QSSN.updateVerificationStatusAndReasonBySSN INVALID (Just req.reason) (ssnEnc & hash)
      ssnEntry <- QSSN.findBySSN (ssnEnc & hash) >>= fromMaybeM (InternalError "SSN not found by ssn no")
      driver <- QDriver.findById ssnEntry.driverId >>= fromMaybeM (PersonNotFound ssnEntry.driverId.getId)
      Notify.notifyDriver merchantOpCityId notificationType (notificationTitle "SSN") (message "SSN") driver driver.deviceToken

postDriverRegistrationDocumentsUpdate :: ShortId DM.Merchant -> Context.City -> Common.UpdateDocumentRequest -> Flow APISuccess
postDriverRegistrationDocumentsUpdate _merchantShortId _opCity _req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case _req of
    Common.Approve approveReq -> handleApproveRequest approveReq merchant.id merchantOpCityId
    Common.Reject rejectReq -> handleRejectRequest rejectReq merchant.id merchantOpCityId
  pure Success

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}

getDriverRegistrationVerificationStatus :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> UTCTime -> UTCTime -> Int -> Int -> Common.DocumentType -> Common.ServiceType -> Flow Common.VerificationStatusListResponse
getDriverRegistrationVerificationStatus _merchantShortId _opCity driverId fromDate toDate limit offset documentType serviceType = do
  let personId = cast @Common.Driver @DP.Person driverId
  _ <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  logDebug $ "serviceType: " <> show serviceType
  logDebug $ "documentType: " <> show documentType
  logDebug $ "personId: " <> show personId
  verificationStatuses <- case serviceType of
    Common.HyperVerge -> do
      entries <- HVQuery.findLatestByDriverIdAndDocType limit offset personId (mapDocumentType documentType) fromDate toDate
      pure $ map (\entry -> convertToVerificationStatusItem entry.status entry.hypervergeResponse entry.retryCount entry.requestId entry.createdAt) entries
    Common.Idfy -> do
      entries <- IDQuery.findLatestByDriverIdAndDocType limit offset personId (mapDocumentType documentType) fromDate toDate
      pure $ map (\entry -> convertToVerificationStatusItem entry.status entry.idfyResponse entry.retryCount entry.requestId entry.createdAt) entries
  logDebug $ "entries IDs: " <> show (map (\entry -> entry.requestId) verificationStatuses)
  pure Common.VerificationStatusListResponse {verificationStatuses = verificationStatuses}
  where
    convertToVerificationStatusItem status verificationMessage retryCount requestId createdAt =
      Common.VerificationStatusItem
        { documentType = documentType,
          status = status,
          verificationMessage = verificationMessage,
          retryCount = fromMaybe 0 retryCount,
          requestId = Just requestId,
          createdAt = createdAt
        }
