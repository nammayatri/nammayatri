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

import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as VC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as Domain hiding (SelfieFetchStatus (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding (convertUTCTimetoDate, removeSpaceAndDash)
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CFQDVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Verification as Verification

data ValidateDocumentImageRequest = ValidateDocumentImageRequest
  { image :: Text,
    imageType :: DVC.DocumentType,
    vehicleCategory :: Maybe VehicleCategory
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ValidateDocumentImageResponse = ValidateDocumentImageResponse
  { imageId :: Id Domain.Image,
    documentNumber :: Maybe Text,
    dateOfBirth :: Maybe Text,
    nameOnCard :: Maybe Text,
    isVerified :: Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDocument ::
  Bool ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ValidateDocumentImageRequest ->
  Flow ValidateDocumentImageResponse
validateDocument isDashboard (personId, merchantId, merchantOpCityId) ValidateDocumentImageRequest {..} = do
  logDebug $ "DocumentRegistration.validateDocument: Starting validation for personId=" <> show personId <> ", imageType=" <> show imageType
  imageResponse <- Image.validateImage isDashboard (personId, merchantId, merchantOpCityId) Image.ImageValidateRequest {image = image, imageType = imageType, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing}
  let imageId :: Id Domain.Image = imageResponse.imageId
  let imageData = image
  logDebug $ "DocumentRegistration.validateDocument: Image validated successfully, imageId=" <> show imageId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  operatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  isImageValidationRequired <- case person.role of
    Person.FLEET_OWNER -> do
      docConfigs <- CFQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId imageType Nothing
      return $ maybe True (.isImageValidationRequired) docConfigs
    _ -> do
      docConfigs <- CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOpCityId imageType (fromMaybe CAR vehicleCategory) Nothing
      return $ maybe True (.isImageValidationRequired) docConfigs
  logDebug $ "DocumentRegistration.validateDocument: isImageValidationRequired=" <> show isImageValidationRequired
  if not isImageValidationRequired
    then do
      return $ ValidateDocumentImageResponse imageId Nothing Nothing Nothing True
    else do
      (documentNumber, dateOfBirth, nameOnCard) <- case imageType of
        DVC.DriverLicense -> do
          resp <- Verification.extractDLImage merchantId merchantOpCityId $ Verification.ExtractImageReq {image1 = imageData, image2 = Nothing, driverId = personId.getId}
          logDebug $ "DocumentRegistration.validateDocument: Extracted DL Image successfully, resp=" <> show resp
          case resp.extractedDL of
            Just extractedDL -> do
              let documentNumber = removeSpaceAndDash <$> extractedDL.dlNumber
              let dateOfBirth = fmap convertUTCTimetoDate (VC.parseDateTime =<< extractedDL.dateOfBirth)
              let nameOnCard = extractedDL.nameOnCard
              DL.cacheExtractedDl personId documentNumber (show operatingCity.city)
              return (documentNumber, dateOfBirth, nameOnCard)
            Nothing -> do
              return (Nothing, Nothing, Nothing)
        DVC.VehicleRegistrationCertificate -> do
          resp <- Verification.extractRCImage merchantId merchantOpCityId $ Verification.ExtractImageReq {image1 = imageData, image2 = Nothing, driverId = personId.getId}
          case resp.extractedRC of
            Just extractedRC -> do
              let documentNumber = removeSpaceAndDash <$> extractedRC.rcNumber
              return (documentNumber, Nothing, Nothing)
            Nothing -> do
              return (Nothing, Nothing, Nothing)
        _ -> return (Nothing, Nothing, Nothing)
      logDebug $ "DocumentRegistration.validateDocument: Validation completed, returning response with documentNumber=" <> show documentNumber <> ", dateOfBirth=" <> show dateOfBirth
      pure $ ValidateDocumentImageResponse imageId documentNumber dateOfBirth nameOnCard True
