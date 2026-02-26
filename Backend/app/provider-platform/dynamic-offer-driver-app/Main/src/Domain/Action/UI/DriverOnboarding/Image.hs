{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.Image
  ( ImageValidateRequest (..),
    ImageValidateResponse (..),
    ImageValidateFileRequest (..),
    validateImage,
    validateImageFile,
    getImage,
    imageS3Lock,
    throwValidationError,
    convertHVStatusToValidationStatus,
    convertValidationStatusToVerificationStatus,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as Domain
import AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate (imageS3Lock)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as Domain hiding (SelfieFetchStatus (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Decode, base64Encode)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Verification.Interface as VI
import Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CFQDVC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverRCAssociation as QDRCA
import qualified Storage.Queries.FleetRCAssociationExtra as FRCA
import qualified Storage.Queries.Image as Query
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Error
import qualified Tools.Verification as Verification
import Utils.Common.Cac.KeyNameConstants

data ImageValidateRequest = ImageValidateRequest
  { image :: Text,
    imageType :: DVC.DocumentType,
    rcNumber :: Maybe Text, -- for PUC, Permit, Insurance and Fitness
    validationStatus :: Maybe Domain.ValidationStatus,
    workflowTransactionId :: Maybe Text,
    vehicleCategory :: Maybe VehicleCategory,
    sdkFailureReason :: Maybe Text, -- used when frontend sdk is used for extraction.
    fileExtension :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ImageValidateFileRequest = ImageValidateFileRequest
  { image :: FilePath,
    imageType :: DVC.DocumentType,
    rcNumber :: Maybe Text, -- for PUC, Permit, Insurance and Fitness
    validationStatus :: Maybe Domain.ValidationStatus,
    workflowTransactionId :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance FromMultipart Tmp ImageValidateFileRequest where
  fromMultipart form = do
    ImageValidateFileRequest
      <$> fmap fdPayload (lookupFile "image" form)
      <*> fmap (read . T.unpack) (lookupInput "imageType" form)
      <*> parseMaybeInput "rcNumber" form
      <*> parseMaybeInput "validationStatus" form
      <*> parseMaybeInput "workflowTransactionId" form

parseMaybeInput :: Read b => Text -> MultipartData tag -> Either String (Maybe b)
parseMaybeInput fieldName form = case lookupInput fieldName form of
  Right val -> Right $ readMaybe (T.unpack val)
  Left _ -> Right Nothing

newtype ImageValidateResponse = ImageValidateResponse
  {imageId :: Id Domain.Image}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data GetDocsResponse = GetDocsResponse
  { dlImage :: Maybe Text,
    rcImage :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

createPath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3Env m)) =>
  Text ->
  Text ->
  DVC.DocumentType ->
  Maybe Text ->
  m Text
createPath driverId merchantId documentType mbExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
      sanitizedExt =
        case mbExtension of
          Nothing -> "png"
          Just ext ->
            ext
              & T.strip
              & T.dropWhile (== '.')
              & (\t -> if T.null t then "png" else T.toLower t)
  return
    ( pathPrefix <> "/driver-onboarding/" <> "org-" <> merchantId <> "/"
        <> driverId
        <> "/"
        <> show documentType
        <> "/"
        <> fileName
        <> "."
        <> sanitizedExt
    )

validateImageHandler ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImageHandler isDashboard mbUploaderRole mbDocConfigs (personId, _, merchantOpCityId) req@ImageValidateRequest {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantId = person.merchantId
  docConfigs <- maybe (CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId imageType Nothing) pure mbDocConfigs
  -- Only restrict when rolesAllowedToUploadDocument is non-empty; Nothing or [] means all roles allowed
  whenJust (listToMaybe docConfigs >>= (.rolesAllowedToUploadDocument)) $ \allowedRoles ->
    when (not $ null allowedRoles) $ do
      let uploaderRole = fromMaybe person.role mbUploaderRole
      unless (uploaderRole `elem` allowedRoles) $
        throwError (InvalidRequesterRole $ show uploaderRole)
  when (isJust validationStatus && imageType == DVC.ProfilePhoto) $ checkIfGenuineReq merchantId req
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  imageSizeInBytes <- fromMaybeM (InvalidRequest "Failed to decode base64 image") $ base64Decode image
  let maxSizeInBytes = fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB * 1024 * 1024 -- Should be set for all merchants, taking 100 if not set
  when (BS.length imageSizeInBytes > maxSizeInBytes) $
    throwError $ InvalidRequest $ "Image size " <> show (BS.length imageSizeInBytes) <> " bytes exceeds maximum limit of " <> show maxSizeInBytes <> " bytes (" <> show (fromMaybe 100 transporterConfig.maxAllowedDocSizeInMB) <> "MB)"
  let rcDependentDocuments = [DVC.VehiclePUC, DVC.VehiclePermit, DVC.VehicleInsurance, DVC.VehicleFitnessCertificate, DVC.VehicleNOC, DVC.VehicleBack, DVC.VehicleBackInterior, DVC.VehicleFront, DVC.VehicleFrontInterior, DVC.VehicleRight, DVC.VehicleLeft, DVC.Odometer, DVC.InspectionHub]
  mbRcId <-
    if imageType `elem` rcDependentDocuments
      then case rcNumber of
        Just rcNo -> do
          rc <- QRC.findLastVehicleRCWrapper rcNo >>= fromMaybeM (RCNotFound rcNo)
          case person.role of
            Person.FLEET_OWNER -> do
              fleetAssoc <- FRCA.findLatestByRCIdAndFleetOwnerId rc.id personId
              when (isNothing fleetAssoc) $ throwError RCNotLinkedWithFleet
              return $ Just rc.id
            _ -> do
              mbAssoc <- QDRCA.findLatestByRCIdAndDriverId rc.id personId
              when (isNothing mbAssoc) $ throwError RCNotLinked
              return $ Just rc.id
        Nothing -> throwError $ RCMandatory (show imageType)
      else return Nothing

  allImages <- Query.findRecentByPersonIdAndImageType personId imageType
  let images = filter ((\txnId -> isNothing txnId || (txnId /= workflowTransactionId)) . (.workflowTransactionId)) allImages
  unless isDashboard $ do
    let onboardingTryLimit = transporterConfig.onboardingTryLimit
    when (length images > onboardingTryLimit * bool 1 2 (imageType == DVC.AadhaarCard || imageType == DVC.DriverLicense)) $ do
      -- not needed now
      driverPhone <- mapM decrypt person.mobileNumber
      notifyErrorToSupport person org.id merchantOpCityId driverPhone org.name ((.failureReason) <$> images)
      throwError (ImageValidationExceedLimit personId.getId)

  -- WorkflowTransactionId is used only in case of hyperverge request
  let mValidatedImage = find ((== Just Documents.VALID) . (.verificationStatus)) images
  case mValidatedImage of
    Just validatedImage
      | imageType /= DVC.DriverLicense,
        isJust workflowTransactionId ->
        return $ ImageValidateResponse validatedImage.id
    _ -> do
      when -- This Condition could be merged with the 1st condition above by replacing images with allImages for mValidatedImage.
        ( imageType == DVC.ProfilePhoto
            && any
              ( \img ->
                  img.verificationStatus == Just Documents.VALID
                    && img.workflowTransactionId == workflowTransactionId
              )
              allImages
        )
        $ throwError $ DocumentAlreadyValidated (show imageType)

      imagePath <- createPath personId.getId merchantId.getId imageType fileExtension
      fork "S3 Put Image" do
        Redis.withLockRedis (imageS3Lock imagePath) 5 $
          S3.put (T.unpack imagePath) image
      imageEntity <- mkImage personId merchantId (Just merchantOpCityId) imagePath imageType mbRcId (convertValidationStatusToVerificationStatus <$> validationStatus) workflowTransactionId sdkFailureReason
      Query.create imageEntity

      -- skipping validation for rc as validation not available in idfy
      isImageValidationRequired <- case person.role of
        Person.FLEET_OWNER -> do
          --------------- Image validation for fleet (different config table than docConfigs)
          fleetDocConfigs <- CFQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId imageType Nothing
          return $ maybe True (.isImageValidationRequired) fleetDocConfigs
        _ -> return $ maybe True (.isImageValidationRequired) $ find (\c -> c.vehicleCategory == fromMaybe CAR vehicleCategory) docConfigs
      if isImageValidationRequired && isNothing validationStatus
        then do
          validationOutput <-
            Verification.validateImage merchantId merchantOpCityId $
              Verification.ValidateImageReq {image, imageType = castImageType imageType, driverId = person.id.getId}
          when validationOutput.validationAvailable $ do
            checkErrors imageEntity.id imageType validationOutput.detectedImage
          Query.updateVerificationStatusOnlyById Documents.VALID imageEntity.id
        else when (isNothing validationStatus) $ Query.updateVerificationStatusOnlyById Documents.MANUAL_VERIFICATION_REQUIRED imageEntity.id
      return $ ImageValidateResponse {imageId = imageEntity.id}
  where
    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just detectedImage) = do
      let outputImageType = detectedImage.imageType
      unless (outputImageType == castImageType imgType) $ throwImageError id_ (ImageInvalidType (show imgType) "")

      unless (fromMaybe False detectedImage.isReadable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) detectedImage.confidence) $
        throwImageError id_ ImageLowQuality

    checkIfGenuineReq :: Id DM.Merchant -> ImageValidateRequest -> Flow ()
    checkIfGenuineReq merchantId request = do
      (txnId, valStatus) <- fromMaybeM (InvalidRequest "Cannot find necessary data for SDK response!!!!") ((,) <$> request.workflowTransactionId <*> request.validationStatus)
      hvResp <- Verification.verifySdkResp merchantId merchantOpCityId (VI.VerifySdkDataReq txnId)
      (respTxnId, respStatus, respUserDetails) <- fromMaybeM (InvalidRequest "Invalid data recieved while validating data.") ((,,) <$> hvResp.transactionId <*> hvResp.status <*> hvResp.userDetails)
      when (respTxnId /= txnId) $ void $ throwValidationError Nothing Nothing Nothing
      when (convertHVStatusToValidationStatus respStatus /= valStatus) $ void $ throwValidationError Nothing Nothing Nothing
      case respUserDetails of
        VI.HVSelfieFlow (VI.SelfieFlow _) -> return ()
        _ -> void $ throwValidationError Nothing Nothing Nothing

validateImage ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) req@ImageValidateRequest {..} = do
  isLocked <- withLockPersonId
  if isLocked
    then do
      finally
        (validateImageHandler isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) req)
        ( do
            Redis.unlockRedis mkLockKey
            logDebug $ "Create Image Lock for PersonId: " <> personId.getId <> " Unlocked"
        )
    else throwError (InternalError $ "Image Upload In Progress")
  where
    withLockPersonId = do
      isLocked <- Redis.tryLockRedis mkLockKey 45
      return isLocked
    mkLockKey = "CreateImageTransaction:PersonId:-" <> personId.getId <> "-ImageType:" <> show imageType

convertHVStatusToValidationStatus :: Text -> Domain.ValidationStatus
convertHVStatusToValidationStatus status =
  case status of
    "auto_approved" -> Domain.AUTO_APPROVED
    "auto_declined" -> Domain.AUTO_DECLINED
    "needs_review" -> Domain.NEEDS_REVIEW
    "manually_declined" -> Domain.DECLINED
    "manually_approved" -> Domain.APPROVED
    _ -> Domain.DECLINED

throwValidationError :: (EsqDBFlow m r, CacheFlow m r) => Maybe (Id Domain.Image) -> Maybe (Id Domain.Image) -> Maybe Text -> m a
throwValidationError imgId1 imgId2 msg = do
  whenJust (imgId1) Query.deleteById
  whenJust (imgId2) Query.deleteById
  throwError $ InvalidRequest $ fromMaybe "Invalid Data !!!!!" msg

convertValidationStatusToVerificationStatus :: Domain.ValidationStatus -> Documents.VerificationStatus
convertValidationStatusToVerificationStatus = \case
  Domain.AUTO_APPROVED -> Documents.VALID
  Domain.AUTO_DECLINED -> Documents.INVALID
  Domain.APPROVED -> Documents.VALID
  Domain.DECLINED -> Documents.INVALID
  Domain.NEEDS_REVIEW -> Documents.MANUAL_VERIFICATION_REQUIRED

castImageType :: DVC.DocumentType -> Verification.ImageType
castImageType DVC.DriverLicense = Verification.DriverLicense
castImageType DVC.VehicleRegistrationCertificate = Verification.VehicleRegistrationCertificate
castImageType DVC.VehiclePermit = Verification.VehiclePermit
castImageType DVC.VehiclePUC = Verification.VehiclePUC
castImageType DVC.VehicleInsurance = Verification.VehicleInsurance
castImageType DVC.VehicleFitnessCertificate = Verification.VehicleFitnessCertificate
castImageType DVC.VehicleNOC = Verification.VehicleNOC
castImageType _ = Verification.VehicleRegistrationCertificate -- Fix Later

validateImageFile ::
  Bool ->
  Maybe Person.Role ->
  Maybe [DVC.DocumentVerificationConfig] ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateFileRequest ->
  Flow ImageValidateResponse
validateImageFile isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) ImageValidateFileRequest {..} = do
  image' <- L.runIO $ base64Encode <$> BS.readFile image
  validateImage isDashboard mbUploaderRole mbDocConfigs (personId, merchantId, merchantOpCityId) $ ImageValidateRequest image' imageType rcNumber validationStatus workflowTransactionId Nothing Nothing Nothing

mkImage ::
  (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id Person.Person ->
  Id DM.Merchant ->
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Text ->
  DVC.DocumentType ->
  Maybe (Id DVRC.VehicleRegistrationCertificate) ->
  Maybe Documents.VerificationStatus ->
  Maybe Text ->
  Maybe Text ->
  m Domain.Image
mkImage personId_ merchantId mbMerchantOpCityId s3Path documentType_ mbRcId verificationStatus workflowTransactionId sdkFailureReason = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Image
      { id,
        personId = personId_,
        merchantId,
        s3Path,
        imageType = documentType_,
        verificationStatus = Just $ fromMaybe Documents.PENDING verificationStatus,
        failureReason = ImageNotValid <$> sdkFailureReason,
        rcId = getId <$> mbRcId,
        workflowTransactionId,
        reviewerEmail = Nothing,
        documentExpiry = Nothing,
        createdAt = now,
        updatedAt = now,
        merchantOperatingCityId = mbMerchantOpCityId
      }

getImage :: Id DM.Merchant -> Id Domain.Image -> Flow Text
getImage merchantId imageId = do
  imageMetadata <- Query.findById imageId
  case imageMetadata of
    Just img | img.merchantId == merchantId -> S3.get $ T.unpack img.s3Path
    _ -> pure T.empty
