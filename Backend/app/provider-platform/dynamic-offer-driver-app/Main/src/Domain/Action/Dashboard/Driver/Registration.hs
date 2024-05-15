{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Registration
  ( documentsList,
    getDocument,
    uploadDocument,
    registerDL,
    registerRC,
    generateAadhaarOtp,
    verifyAadhaarOtp,
    auth,
    verify,
    underReviewDriversList,
    driverDocumentInfo,
    updateDocument,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import Domain.Action.Dashboard.Driver (castVehicleVariant)
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Action.UI.Registration as DReg
import qualified Domain.Types.DocumentVerificationConfig as Domain
import qualified Domain.Types.FleetDriverAssociation as FDV
import qualified Domain.Types.IdfyVerification as VerificationT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.VehicleFitnessCertificate as DFC
import qualified Domain.Types.VehicleInsurance as DVI
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Environment
import EulerHS.Prelude hiding (map)
import Kernel.Beam.Functions
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.External.Encryption (decrypt, encrypt, hash)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as DomainRC
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverSSN as QSSN
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QDriver
import qualified Storage.Queries.VehicleFitnessCertificate as QFC
import qualified Storage.Queries.VehicleInsurance as QVI
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import qualified Tools.AadhaarVerification as AadhaarVerification
import Tools.Error
import Tools.Notifications as Notify

documentsList :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow Common.DocumentsListResponse
documentsList merchantShortId _ driverId = do
  merchant <- findMerchantByShortId merchantShortId
  licImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.DriverLicense)
  vehRegImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleRegistrationCertificate)
  uploadProfImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.UploadProfile)
  vehicleFitnessCertImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleFitnessCertificate)
  vehicleInsImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) Domain.VehicleInsurance)
  ssnEntry <- QSSN.findByDriverId (cast driverId) >>= fromMaybeM (InternalError $ "SSN not found for driverid: " <> driverId.getId)
  ssnUnenc <- decrypt ssnEntry.ssn
  pure
    Common.DocumentsListResponse
      { driverLicense = licImgs,
        vehicleRegistrationCertificate = vehRegImgs,
        vehicleInsurance = vehicleInsImgs,
        uploadProfile = uploadProfImgs,
        ssn = ssnUnenc,
        vehicleFitnessCertificate = vehicleFitnessCertImgs
      }

getDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Image -> Flow Common.GetDocumentResponse
getDocument merchantShortId _ imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapDocumentType :: Common.DocumentType -> Domain.DocumentType
mapDocumentType Common.DriverLicense = Domain.DriverLicense
mapDocumentType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate
mapDocumentType Common.VehiclePUCImage = Domain.VehiclePUC
mapDocumentType Common.VehiclePermitImage = Domain.VehiclePermit
mapDocumentType Common.VehicleInsuranceImage = Domain.VehicleInsurance
mapDocumentType Common.VehicleFitnessCertificateImage = Domain.VehicleFitnessCertificate
mapDocumentType Common.PanCard = Domain.PanCard

uploadDocument :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
uploadDocument merchantShortId opCity driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  res <-
    validateImage
      True
      (cast driverId_, cast merchant.id, merchantOpCityId)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapDocumentType req.imageType,
          rcNumber = req.rcNumber,
          vehicleCategory = Nothing
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
registerDL merchantShortId opCity driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  verifyDL
    True
    (Just merchant)
    (cast driverId_, cast merchant.id, merchantOpCityId)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        vehicleCategory = Nothing,
        ..
      }

registerRC :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
registerRC merchantShortId opCity driverId_ Common.RegisterRCReq {..} = do
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

generateAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId opCity driverId_ req = do
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

verifyAadhaarOtp :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId opCity driverId_ req = do
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

auth :: ShortId DM.Merchant -> Context.City -> Common.AuthReq -> Flow Common.AuthRes
auth merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    DReg.auth
      True
      DReg.AuthReq
        { mobileNumber = Just req.mobileNumber,
          mobileCountryCode = Just req.mobileCountryCode,
          merchantId = merchant.id.getId,
          merchantOperatingCity = Just opCity,
          registrationLat = Nothing,
          registrationLon = Nothing,
          name = Nothing,
          email = Nothing,
          identifierType = Just SP.MOBILENUMBER
        }
      Nothing
      Nothing
      Nothing
      Nothing
  pure $ Common.AuthRes {authId = res.authId.getId, attempts = res.attempts}

verify :: Text -> Bool -> Text -> Common.AuthVerifyReq -> Flow APISuccess
verify authId mbFleet fleetOwnerId req = do
  let regId = Id authId :: Id SR.RegistrationToken
  res <-
    DReg.verify
      regId
      DReg.AuthVerifyReq
        { otp = req.otp,
          deviceToken = req.deviceToken,
          whatsappNotificationEnroll = Nothing
        }
  when mbFleet $ do
    assoc <- FDV.makeFleetDriverAssociation res.person.id fleetOwnerId (DomainRC.convertTextToUTC (Just "2099-12-12"))
    QFDV.create assoc
  pure Success

underReviewDriversList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Flow Common.UnderReviewDriversListResponse
underReviewDriversList _merchantShortId _opCity _limit _offset = throwError (InternalError "Not Implemented")

driverDocumentInfo :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow [Common.DriverDocument]
driverDocumentInfo _merchantShortId _opCity _driverId = throwError (InternalError "Not Implemented")

updateDocument :: ShortId DM.Merchant -> Context.City -> Common.UpdateDocumentRequest -> Flow APISuccess
updateDocument _merchantShortId _opCity _req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case _req of
    Common.Approve approveReq -> do
      case approveReq of
        Common.RC rcApproveReq -> approveAndUpdateRC rcApproveReq
        Common.DL imageId -> approveAndUpdateDL imageId
        Common.UploadProfile imageId -> QImage.updateToValid True (Id imageId.getId)
        Common.SSNApprove ssnNum -> do
          ssnEnc <- encrypt ssnNum
          QSSN.updateVerificationStatusAndReasonBySSN VerificationT.VALID Nothing (ssnEnc & hash)
        Common.VehicleInsurance vInsuranceReq -> approveAndUpdateInsurance vInsuranceReq
        Common.VehicleFitnessCertificate fitnessReq -> approveAndUpdateFitnessCertificate fitnessReq
        Common.ProfilePhoto imageId -> QImage.updateToValid True (Id imageId.getId)
        _ -> throwError (InternalError "Unknown Config in approve update document")
    Common.Reject rejectReq -> do
      case rejectReq of
        Common.SSNReject ssnRejectReq -> rejectSSNAndSendNotification ssnRejectReq merchantOpCityId
        Common.ImageDocuments imageRejectReq -> do
          let imageId = Id imageRejectReq.documentImageId.getId
          image <- QImage.findById imageId >>= fromMaybeM (InternalError "Image not found by image id")
          case image.imageType of
            Domain.ProfilePhoto -> QImage.updateIsValidAndFailureReason False (Just $ ImageNotValid imageRejectReq.reason) imageId
            Domain.UploadProfile -> QImage.updateIsValidAndFailureReason False (Just $ ImageNotValid imageRejectReq.reason) imageId
            Domain.DriverLicense -> QDL.updateVerificationStatus VerificationT.INVALID imageId
            Domain.VehicleRegistrationCertificate -> QRC.updateVerificationStatus VerificationT.INVALID imageId
            Domain.VehicleFitnessCertificate -> QFC.updateVerificationStatus VerificationT.INVALID imageId
            Domain.VehicleInsurance -> QVI.updateVerificationStatus VerificationT.INVALID imageId
            _ -> throwError (InternalError "Unknown Config in reject update document")
          driver <- QDriver.findById image.personId >>= fromMaybeM (PersonNotFound image.personId.getId)
          Notify.notifyDriver merchantOpCityId notificationType (notificationTitle (show image.imageType)) (message (show image.imageType)) driver driver.deviceToken
  pure Success
  where
    notificationType = FCM.TRIGGER_SERVICE
    notificationTitle obj = "Attention: Your " <> obj <> " is invalid."
    message obj = "Kindly reapply or reupload your " <> obj
    approveAndUpdateDL imageId = do
      QDL.updateVerificationStatus VerificationT.VALID (Id imageId.getId)
      QImage.updateToValid True (Id imageId.getId)
    approveAndUpdateRC req = do
      let imageId = Id req.documentImageId.getId
      rc <- QRC.findByImageId imageId >>= fromMaybeM (InternalError "RC not found by image id")
      let udpatedRC =
            rc
              { DRC.vehicleVariant = (castVehicleVariant <$> req.vehicleVariant) <|> rc.vehicleVariant,
                DRC.verificationStatus = VerificationT.VALID
              }
      QRC.updateByPrimaryKey udpatedRC
      QImage.updateToValid True imageId
    approveAndUpdateInsurance req = do
      let imageId = Id req.documentImageId.getId
      vinsurance <- QVI.findByImageId imageId >>= fromMaybeM (InternalError "Vehicle insurance not found by image id")
      policyNo <- encrypt req.policyNumber
      let updatedInsurance =
            vinsurance
              { DVI.insuredName = req.insuredName <|> vinsurance.insuredName,
                DVI.issueDate = req.issueDate <|> vinsurance.issueDate,
                DVI.limitsOfLiability = req.limitsOfLiability <|> vinsurance.limitsOfLiability,
                DVI.policyExpiry = req.policyExpiry,
                DVI.policyNumber = policyNo,
                DVI.policyProvider = req.policyProvider,
                DVI.verificationStatus = VerificationT.VALID
              }
      QVI.updateByPrimaryKey updatedInsurance
      QImage.updateToValid True imageId
    approveAndUpdateFitnessCertificate req = do
      let imageId = Id req.documentImageId.getId
      fitnessCert <- QFC.findByImageId imageId >>= fromMaybeM (InternalError "Fitness Certificate not found by image id")
      applicationNo <- encrypt req.applicationNumber
      let updatedFitnessCert =
            fitnessCert
              { DFC.applicationNumber = applicationNo,
                DFC.categoryOfVehicle = req.categoryOfVehicle <|> fitnessCert.categoryOfVehicle,
                DFC.fitnessExpiry = req.fitnessExpiry,
                DFC.inspectingAuthority = req.inspectingAuthority <|> fitnessCert.inspectingAuthority,
                DFC.inspectingOn = req.inspectingOn <|> fitnessCert.inspectingOn,
                DFC.nextInspectionDate = req.nextInspectionDate <|> fitnessCert.nextInspectionDate,
                DFC.receiptDate = req.receiptDate <|> fitnessCert.receiptDate,
                DFC.verificationStatus = VerificationT.VALID
              }
      QFC.updateByPrimaryKey updatedFitnessCert
      QImage.updateToValid True imageId
    rejectSSNAndSendNotification req merchantOpCityId = do
      ssnEnc <- encrypt req.ssn
      QSSN.updateVerificationStatusAndReasonBySSN VerificationT.INVALID (Just req.reason) (ssnEnc & hash)
      ssnEntry <- QSSN.findBySSN (ssnEnc & hash) >>= fromMaybeM (InternalError "SSN not found by ssn no")
      driver <- QDriver.findById ssnEntry.driverId >>= fromMaybeM (PersonNotFound ssnEntry.driverId.getId)
      Notify.notifyDriver merchantOpCityId notificationType (notificationTitle "SSN") (message "SSN") driver driver.deviceToken

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}
