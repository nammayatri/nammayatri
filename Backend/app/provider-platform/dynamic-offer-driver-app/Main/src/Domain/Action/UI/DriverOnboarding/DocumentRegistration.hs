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
    getOCRResultRC,
    getOCRResultDL,
    getOCRResultPAN,
  )
where

import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DL
import qualified Domain.Action.UI.DriverOnboarding.Image as Image
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as Domain hiding (SelfieFetchStatus (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import Environment
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.DriverOnboarding (convertUTCTimetoDate, parseDateTime, preProcessDocumentIdentifier, removeSpaceAndDash)
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
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
    isVerified :: Bool,
    -- | RC-specific OCR fields (populated when imageType = VehicleRegistrationCertificate)
    vehicleClass :: Maybe Text,
    manufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    fuelType :: Maybe Text,
    colour :: Maybe Text,
    chassisNumber :: Maybe Text,
    engineNumber :: Maybe Text,
    registrationDate :: Maybe Text,
    ownerName :: Maybe Text,
    ocrProvider :: Maybe VT.VerificationService
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDocument ::
  Bool ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ValidateDocumentImageRequest ->
  Flow ValidateDocumentImageResponse
validateDocument isDashboard (personId, merchantId, merchantOpCityId) ValidateDocumentImageRequest {..} = do
  logDebug $ "DocumentRegistration.validateDocument: Starting validation for personId=" <> show personId <> ", imageType=" <> show imageType
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  imageResponse <- Image.validateImage isDashboard Nothing Nothing (personId, merchantId, merchantOpCityId) Image.ImageValidateRequest {image = image, imageType = imageType, rcNumber = Nothing, validationStatus = Nothing, workflowTransactionId = Nothing, vehicleCategory = Nothing, sdkFailureReason = Nothing, fileExtension = Nothing}
  let imageId :: Id Domain.Image = imageResponse.imageId
  let imageData = image
  logDebug $ "DocumentRegistration.validateDocument: Image validated successfully, imageId=" <> show imageId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  operatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  isImageValidationRequired <-
    if person.role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS]
      then do
        -- Role-aware fleet config (in-mem cached); default to requiring validation when none exists.
        mbDocConfig <- SStatus.findFleetDocVerificationConfig merchantOpCityId imageType person.role
        return $ maybe True (.isImageValidationRequired) mbDocConfig
      else do
        docConfigs <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just imageType, vehicleCategory = Just (fromMaybe CAR vehicleCategory)}) Nothing
        return $ maybe True (.isImageValidationRequired) docConfigs
  logDebug $ "DocumentRegistration.validateDocument: isImageValidationRequired=" <> show isImageValidationRequired
  if not isImageValidationRequired
    then do
      return $ emptyValidateDocumentImageResponse imageId
    else do
      case imageType of
        DVC.DriverLicense -> do
          resp <- Verification.extractDLImage merchantId merchantOpCityId $ Verification.ExtractImageReq {image1 = imageData, image2 = Nothing, driverId = personId.getId}
          logDebug $ "DocumentRegistration.validateDocument: Extracted DL Image successfully, resp=" <> show resp
          if resp.provider == Just VT.InternalOCR
            then return $ (emptyValidateDocumentImageResponse imageId) {ocrProvider = Just VT.InternalOCR}
            else case resp.extractedDL of
              Just extractedDL -> do
                let documentNumber = preProcessDocumentIdentifier transporterConfig <$> extractedDL.dlNumber
                let dateOfBirth = fmap convertUTCTimetoDate (parseDateTime =<< extractedDL.dateOfBirth)
                let nameOnCard = extractedDL.nameOnCard
                DL.cacheExtractedDl personId documentNumber (show operatingCity.city)
                DL.cacheExtractedDlName personId nameOnCard
                logDebug $ "DocumentRegistration.validateDocument: Validation completed, returning response with documentNumber=" <> show documentNumber <> ", dateOfBirth=" <> show dateOfBirth
                pure $ (emptyValidateDocumentImageResponse imageId) {documentNumber, dateOfBirth, nameOnCard}
              Nothing ->
                return $ emptyValidateDocumentImageResponse imageId
        DVC.VehicleRegistrationCertificate -> do
          resp <- Verification.extractRCImage merchantId merchantOpCityId $ Verification.ExtractImageReq {image1 = imageData, image2 = Nothing, driverId = personId.getId}
          if resp.provider == Just VT.InternalOCR
            then return $ (emptyValidateDocumentImageResponse imageId) {ocrProvider = Just VT.InternalOCR}
            else case resp.extractedRC of
              Just extractedRC -> do
                let documentNumber = preProcessDocumentIdentifier transporterConfig <$> extractedRC.rcNumber
                logDebug $ "DocumentRegistration.validateDocument: RC OCR completed, rcNumber=" <> show documentNumber
                pure $
                  (emptyValidateDocumentImageResponse imageId)
                    { documentNumber,
                      vehicleClass = extractedRC.vehicleClass,
                      manufacturer = extractedRC.manufacturer,
                      vehicleModel = extractedRC.model,
                      fuelType = extractedRC.fuelType,
                      colour = extractedRC.colour,
                      chassisNumber = extractedRC.chassisNumber,
                      engineNumber = extractedRC.engineNumber,
                      registrationDate = extractedRC.registrationDate,
                      ownerName = extractedRC.ownerName
                    }
              Nothing ->
                return $ emptyValidateDocumentImageResponse imageId
        DVC.PanCard -> do
          resp <- Verification.extractPanImage merchantId merchantOpCityId $ Verification.ExtractImageReq {image1 = imageData, image2 = Nothing, driverId = personId.getId}
          if resp.provider == Just VT.InternalOCR
            then return $ (emptyValidateDocumentImageResponse imageId) {ocrProvider = Just VT.InternalOCR}
            else case resp.extractedPan of
              Just extractedPan -> do
                let documentNumber = removeSpaceAndDash <$> extractedPan.id_number
                let nameOnCard = extractedPan.name_on_card
                let dateOfBirth = fmap convertUTCTimetoDate (parseDateTime =<< extractedPan.date_of_birth)
                pure $ (emptyValidateDocumentImageResponse imageId) {documentNumber, nameOnCard, dateOfBirth}
              Nothing ->
                return $ emptyValidateDocumentImageResponse imageId
        _ -> return $ emptyValidateDocumentImageResponse imageId

emptyValidateDocumentImageResponse :: Id Domain.Image -> ValidateDocumentImageResponse
emptyValidateDocumentImageResponse imageId =
  ValidateDocumentImageResponse
    { imageId,
      documentNumber = Nothing,
      dateOfBirth = Nothing,
      nameOnCard = Nothing,
      isVerified = True,
      vehicleClass = Nothing,
      manufacturer = Nothing,
      vehicleModel = Nothing,
      fuelType = Nothing,
      colour = Nothing,
      chassisNumber = Nothing,
      engineNumber = Nothing,
      registrationDate = Nothing,
      ownerName = Nothing,
      ocrProvider = Nothing
    }

getOCRResultRC ::
  Id Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Flow ValidateDocumentImageResponse
getOCRResultRC personId merchantOpCityId mbImageId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mbRC <- Verification.getOCRResultRC personId.getId
  whenJust mbRC $ \_ ->
    Hedis.withCrossAppRedis $ Hedis.del ("providerPlatform:InternalOCR:RegistrationCertificate:" <> personId.getId)
  let resolvedImageId = maybe (Id "") Id mbImageId
  case mbRC of
    Nothing -> pure $ emptyValidateDocumentImageResponse resolvedImageId
    Just rc ->
      pure $
        (emptyValidateDocumentImageResponse resolvedImageId)
          { documentNumber = preProcessDocumentIdentifier transporterConfig <$> rc.rcNumber,
            vehicleClass = rc.vehicleClass,
            manufacturer = rc.manufacturer,
            vehicleModel = rc.model,
            fuelType = rc.fuelType,
            colour = rc.colour,
            chassisNumber = rc.chassisNumber,
            engineNumber = rc.engineNumber,
            registrationDate = rc.registrationDate,
            ownerName = rc.ownerName,
            ocrProvider = Just VT.InternalOCR
          }

getOCRResultDL ::
  Id Person.Person ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Flow ValidateDocumentImageResponse
getOCRResultDL personId merchantOpCityId mbImageId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mbDL <- Verification.getOCRResultDL personId.getId
  whenJust mbDL $ \_ ->
    Hedis.withCrossAppRedis $ Hedis.del ("providerPlatform:InternalOCR:DriverLicense:" <> personId.getId)
  let resolvedImageId = maybe (Id "") Id mbImageId
  case mbDL of
    Nothing -> pure $ emptyValidateDocumentImageResponse resolvedImageId
    Just dl -> do
      operatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      let documentNumber = preProcessDocumentIdentifier transporterConfig <$> dl.dlNumber
      let dateOfBirth = fmap convertUTCTimetoDate (parseDateTime =<< dl.dateOfBirth)
      let nameOnCard = dl.nameOnCard
      DL.cacheExtractedDl personId documentNumber (show operatingCity.city)
      DL.cacheExtractedDlName personId nameOnCard
      pure $
        (emptyValidateDocumentImageResponse resolvedImageId)
          { documentNumber,
            nameOnCard,
            dateOfBirth,
            ocrProvider = Just VT.InternalOCR
          }

getOCRResultPAN ::
  Id Person.Person ->
  Maybe Text ->
  Flow ValidateDocumentImageResponse
getOCRResultPAN personId mbImageId = do
  mbPAN <- Verification.getOCRResultPAN personId.getId
  whenJust mbPAN $ \_ ->
    Hedis.withCrossAppRedis $ Hedis.del ("providerPlatform:InternalOCR:PanCard:" <> personId.getId)
  let resolvedImageId = maybe (Id "") Id mbImageId
  case mbPAN of
    Nothing -> pure $ emptyValidateDocumentImageResponse resolvedImageId
    Just pan ->
      pure $
        (emptyValidateDocumentImageResponse resolvedImageId)
          { documentNumber = removeSpaceAndDash <$> pan.panNumber,
            nameOnCard = pan.nameOnCard,
            dateOfBirth = fmap convertUTCTimetoDate (parseDateTime =<< pan.dateOfBirth),
            ocrProvider = Just VT.InternalOCR
          }
