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
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver.Registration as Common
import qualified Domain.Action.UI.DriverOnboarding.AadhaarVerification as AV
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.DriverOnboarding.Image
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.FleetDriverAssociation as FDV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RegistrationToken as SR
import Environment
import Kernel.Beam.Functions
import Kernel.External.AadhaarVerification.Interface.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Queries.DriverOnboarding.Image as QImage
import qualified Storage.Queries.FleetDriverAssociation as QFDV
import qualified Tools.AadhaarVerification as AadhaarVerification

documentsList :: ShortId DM.Merchant -> Id Common.Driver -> Flow Common.DocumentsListResponse
documentsList merchantShortId driverId = do
  merchant <- findMerchantByShortId merchantShortId
  licImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) DriverLicense)
  vehRegImgs <- map (.id.getId) <$> runInReplica (findImagesByPersonAndType merchant.id (cast driverId) VehicleRegistrationCertificate)
  pure
    Common.DocumentsListResponse
      { driverLicense = licImgs,
        vehicleRegistrationCertificate = vehRegImgs
      }

getDocument :: ShortId DM.Merchant -> Id Common.Image -> Flow Common.GetDocumentResponse
getDocument merchantShortId imageId = do
  merchant <- findMerchantByShortId merchantShortId
  img <- getImage merchant.id (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapImageType :: Common.DocumentType -> Domain.ImageType
mapImageType Common.DriverLicense = Domain.DriverLicense
mapImageType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate

uploadDocument :: ShortId DM.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
uploadDocument merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    validateImage
      True
      (cast driverId_, cast merchant.id)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapImageType req.imageType
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
registerDL merchantShortId driverId_ Common.RegisterDLReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  verifyDL
    True
    (Just merchant)
    (cast driverId_, cast merchant.id)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        ..
      }

registerRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
registerRC merchantShortId driverId_ Common.RegisterRCReq {..} = do
  merchant <- findMerchantByShortId merchantShortId
  verifyRC
    True
    (Just merchant)
    (cast driverId_, cast merchant.id)
    ( DriverRCReq
        { imageId = cast imageId,
          ..
        }
    )
    Nothing

generateAadhaarOtp :: ShortId DM.Merchant -> Id Common.Driver -> Common.GenerateAadhaarOtpReq -> Flow Common.GenerateAadhaarOtpRes
generateAadhaarOtp merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    AV.generateAadhaarOtp
      True
      (Just merchant)
      (cast driverId_)
      AadhaarVerification.AadhaarOtpReq
        { aadhaarNumber = req.aadhaarNumber,
          consent = req.consent
        }
  pure (convertVerifyOtp res)

verifyAadhaarOtp :: ShortId DM.Merchant -> Id Common.Driver -> Common.VerifyAadhaarOtpReq -> Flow Common.VerifyAadhaarOtpRes
verifyAadhaarOtp merchantShortId driverId_ req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    AV.verifyAadhaarOtp
      (Just merchant)
      (cast driverId_)
      AV.VerifyAadhaarOtpReq
        { otp = req.otp,
          shareCode = req.shareCode
        }
  pure (convertSubmitOtp res)

auth :: ShortId DM.Merchant -> Common.AuthReq -> Flow Common.AuthRes
auth merchantShortId req = do
  merchant <- findMerchantByShortId merchantShortId
  res <-
    DReg.auth
      True
      DReg.AuthReq
        { mobileNumber = req.mobileNumber,
          mobileCountryCode = req.mobileCountryCode,
          merchantId = merchant.id.getId
        }
      Nothing
      Nothing
  pure $ Common.AuthRes {authId = res.authId.getId, attempts = res.attempts}

verify :: Text -> Common.AuthVerifyReq -> Bool -> Text -> Flow APISuccess
verify authId req mbFleet fleetOwnerId = do
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
    assoc <- FDV.makeFleetVehicleDriverAssociation res.person.id fleetOwnerId
    QFDV.create assoc
  pure Success

convertVerifyOtp :: AadhaarVerificationResp -> Common.GenerateAadhaarOtpRes
convertVerifyOtp AadhaarVerificationResp {..} = Common.GenerateAadhaarOtpRes {..}

convertSubmitOtp :: AadhaarOtpVerifyRes -> Common.VerifyAadhaarOtpRes
convertSubmitOtp AadhaarOtpVerifyRes {..} = Common.VerifyAadhaarOtpRes {..}
