{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.DocumentRegistration
  ( validateDocument,
    ValidateDocumentImageRequest (..),
    ValidateDocumentImageResponse (..),
  )
where

import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as Domain hiding (SelfieFetchStatus (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

data ValidateDocumentImageRequest = ValidateDocumentImageRequest
  { image :: Text,
    imageType :: DVC.DocumentType,
    documentInfoText :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ValidateDocumentImageResponse = ValidateDocumentImageResponse
  { imageId :: Id Domain.Image,
    documentNumber :: Maybe Text,
    dateOfBirth :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDocument ::
  Bool ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ValidateDocumentImageRequest ->
  Flow ValidateDocumentImageResponse
validateDocument isDashboard (personId, merchantId, merchantOpCityId) ValidateDocumentImageRequest {..} = do
  logDebug $ "DocumentRegistration.validateDocument: Starting validation for personId=" <> show personId <> ", imageType=" <> show imageType <> ", hasDocumentInfoText=" <> show (isJust documentInfoText)
  imageResponse <- Image.validateImage isDashboard (personId, merchantId, merchantOpCityId) Image.ImageValidateRequest {image = image, imageType = imageType, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
  let imageId = imageResponse.imageId
  logDebug $ "DocumentRegistration.validateDocument: Image validated successfully, imageId=" <> show imageId
  (documentNumber, dateOfBirth) <- case documentInfoText of
    Just docText -> do
      logDebug $ "DocumentRegistration.validateDocument: Extracting info from documentInfoText, imageType=" <> show imageType
      case imageType of
        DVC.DriverLicense -> do
          logDebug $ "DocumentRegistration.validateDocument: Extracting DL info"
          (documentNumber, dateOfBirth) <- Image.extractDLInfoFromText merchantId merchantOpCityId docText
          logDebug $ "DocumentRegistration.validateDocument: DL extraction completed, documentNumber=" <> show documentNumber <> ", dateOfBirth=" <> show dateOfBirth
          return (documentNumber, dateOfBirth)
        DVC.VehicleRegistrationCertificate -> do
          logDebug $ "DocumentRegistration.validateDocument: Extracting RC info"
          documentNumber <- Image.extractRCInfoFromText merchantId merchantOpCityId docText
          logDebug $ "DocumentRegistration.validateDocument: RC extraction completed, documentNumber=" <> show documentNumber
          return (documentNumber, Nothing)
        _ -> do
          logDebug $ "DocumentRegistration.validateDocument: Unsupported document type for auto-registration: " <> show imageType
          return (Nothing, Nothing) -- Only DL and RC are supported for auto-registration
    Nothing -> do
      logDebug $ "DocumentRegistration.validateDocument: No documentInfoText provided, skipping extraction"
      return (Nothing, Nothing)
  logDebug $ "DocumentRegistration.validateDocument: Validation completed, returning response with documentNumber=" <> show documentNumber <> ", dateOfBirth=" <> show dateOfBirth
  pure $ ValidateDocumentImageResponse imageId documentNumber dateOfBirth
