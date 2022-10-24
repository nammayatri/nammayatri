module Domain.Action.Dashboard.Driver.Registration where

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
import Environment
import Storage.Queries.DriverOnboarding.Image as QImage

documentsList :: Id Common.Driver -> FlowHandler Common.DocumentsListResponse
documentsList driverId = withFlowHandlerAPI $ do
  licImgs <- map (.id.getId) <$> findImagesByPersonAndType (cast driverId) DriverLicense
  vehRegImgs <- map (.id.getId) <$> findImagesByPersonAndType (cast driverId) VehicleRegistrationCertificate
  pure
    Common.DocumentsListResponse
      { driverLicense = licImgs,
        vehicleRegistrationCertificate = vehRegImgs
      }

getDocument :: Id Common.Image -> FlowHandler Common.GetDocumentResponse
getDocument imageId = withFlowHandlerAPI $ do
  img <- getImage (cast imageId)
  pure Common.GetDocumentResponse {imageBase64 = img}

mapImageType :: Common.DocumentType -> Domain.ImageType
mapImageType Common.DriverLicense = Domain.DriverLicense
mapImageType Common.VehicleRegistrationCertificate = Domain.VehicleRegistrationCertificate

uploadDocument :: Id Common.Driver -> Common.UploadDocumentReq -> FlowHandler Common.UploadDocumentResp
uploadDocument driverId_ req = withFlowHandlerAPI $ do
  res <-
    validateImage
      True
      (cast driverId_)
      ImageValidateRequest
        { image = req.imageBase64,
          imageType = mapImageType req.imageType
        }
  pure $ Common.UploadDocumentResp {imageId = cast res.imageId}

registerDL :: Id Common.Driver -> Common.RegisterDLReq -> FlowHandler APISuccess
registerDL driverId_ Common.RegisterDLReq {..} = withFlowHandlerAPI $ do
  verifyDL
    True
    (cast driverId_)
    DriverDLReq
      { imageId1 = cast imageId1,
        imageId2 = fmap cast imageId2,
        ..
      }

registerRC :: Id Common.Driver -> Common.RegisterRCReq -> FlowHandler APISuccess
registerRC driverId_ Common.RegisterRCReq {..} = withFlowHandlerAPI $ do
  verifyRC
    True
    (cast driverId_)
    DriverRCReq
      { imageId = cast imageId,
        ..
      }
