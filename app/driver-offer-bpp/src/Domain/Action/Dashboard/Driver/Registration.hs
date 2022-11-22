module Domain.Action.Dashboard.Driver.Registration
  ( documentsList,
    getDocument,
    uploadDocument,
    registerDL,
    registerRC,
  )
where

import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import Dashboard.Common.Driver.Registration (GetDocumentResponse (imageBase64))
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import Domain.Action.UI.DriverOnboarding.DriverLicense
import Domain.Action.UI.DriverOnboarding.Image
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.DriverOnboarding.Image
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Merchant as DM
import Environment
import qualified Storage.CachedQueries.Merchant as QM
import Storage.Queries.DriverOnboarding.Image as QImage
import Tools.Error

documentsList :: ShortId DM.Merchant -> Id Common.Driver -> Flow Common.DocumentsListResponse
documentsList merchantShortId driverId = do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  licImgs <- map (.id.getId) <$> findImagesByPersonAndType merchant.id (cast driverId) DriverLicense
  vehRegImgs <- map (.id.getId) <$> findImagesByPersonAndType merchant.id (cast driverId) VehicleRegistrationCertificate
  pure
    Common.DocumentsListResponse
      { driverLicense = licImgs,
        vehicleRegistrationCertificate = vehRegImgs
      }

getDocument :: ShortId DM.Merchant -> Id Common.Image -> Flow Common.GetDocumentResponse
getDocument merchantShortId imageId = do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  img <- getImage merchant.id (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapImageType :: Common.DocumentType -> Domain.ImageType
mapImageType Common.DriverLicense = Domain.DriverLicense
mapImageType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate

uploadDocument :: ShortId DM.Merchant -> Id Common.Driver -> Common.UploadDocumentReq -> Flow Common.UploadDocumentResp
uploadDocument merchantShortId driverId_ req = do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  res <-
    validateImage
      True
      (Just merchant)
      (cast driverId_)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapImageType req.imageType
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterDLReq -> Flow APISuccess
registerDL merchantShortId driverId_ Common.RegisterDLReq {..} = do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  verifyDL
    True
    (Just merchant)
    (cast driverId_)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        ..
      }

registerRC :: ShortId DM.Merchant -> Id Common.Driver -> Common.RegisterRCReq -> Flow APISuccess
registerRC merchantShortId driverId_ Common.RegisterRCReq {..} = do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  verifyRC
    True
    (Just merchant)
    (cast driverId_)
    DriverRCReq
      { imageId = cast imageId,
        ..
      }
