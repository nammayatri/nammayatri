{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.Image where

import AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Multipart
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.TransporterConfig
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Storage.Queries.Person as Person
import qualified Tools.Verification as Verification

data ImageValidateRequest = ImageValidateRequest
  { image :: Text,
    imageType :: Domain.ImageType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data ImageValidateFileRequest = ImageValidateFileRequest
  { image :: FilePath,
    imageType :: Domain.ImageType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance FromMultipart Tmp ImageValidateFileRequest where
  fromMultipart form = do
    ImageValidateFileRequest
      <$> fmap fdPayload (lookupFile "image" form)
      <*> fmap (read . T.unpack) (lookupInput "imageType" form)

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
  Domain.ImageType ->
  m Text
createPath driverId merchantId imageType = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/driver-onboarding/" <> "org-" <> merchantId <> "/"
        <> driverId
        <> "/"
        <> show imageType
        <> "/"
        <> fileName
        <> ".png"
    )

validateImage ::
  Bool ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage isDashboard (personId, _, merchantOpCityId) ImageValidateRequest {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantId = person.merchantId
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- not needed org now because it is used to notify
  -- org <- case mbMerchant of
  --   Nothing -> do
  --     CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  --   Just merchant -> do
  --     -- merchant access checking
  --     unless (merchant.id == merchantId) $ throwError (PersonNotFound personId.getId)
  --     pure merchant

  images <- Query.findRecentByPersonIdAndImageType personId merchantOpCityId imageType
  unless isDashboard $ do
    transporterConfig <- findByMerchantOpCityId merchantOpCityId 0 Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    let onboardingTryLimit = transporterConfig.onboardingTryLimit
    when (length images > onboardingTryLimit) $ do
      -- not needed now
      driverPhone <- mapM decrypt person.mobileNumber
      notifyErrorToSupport person org.id merchantOpCityId driverPhone org.name ((.failureReason) <$> images)
      throwError (ImageValidationExceedLimit personId.getId)

  imagePath <- createPath personId.getId merchantId.getId imageType
  _ <- fork "S3 Put Image" $ S3.put (T.unpack imagePath) image
  imageEntity <- mkImage personId merchantId imagePath imageType False
  _ <- Query.create imageEntity

  -- skipping validation for rc as validation not available in idfy
  validationOutput <-
    Verification.validateImage merchantId merchantOpCityId $
      Verification.ValidateImageReq {image, imageType = castImageType imageType, driverId = person.id.getId}
  when validationOutput.validationAvailable $ do
    checkErrors imageEntity.id imageType validationOutput.detectedImage
  _ <- Query.updateToValid imageEntity.id

  return $ ImageValidateResponse {imageId = imageEntity.id}
  where
    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just detectedImage) = do
      let outputImageType = detectedImage.imageType
      unless (outputImageType == castImageType imgType) $ throwImageError id_ (ImageInvalidType (show imgType) (show outputImageType))

      unless (fromMaybe False detectedImage.isReadable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) detectedImage.confidence) $
        throwImageError id_ ImageLowQuality

castImageType :: Domain.ImageType -> Verification.ImageType
castImageType Domain.DriverLicense = Verification.DriverLicense
castImageType Domain.VehicleRegistrationCertificate = Verification.VehicleRegistrationCertificate

validateImageFile ::
  Bool ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ImageValidateFileRequest ->
  Flow ImageValidateResponse
validateImageFile isDashboard (personId, merchantId, merchantOpCityId) ImageValidateFileRequest {..} = do
  image' <- L.runIO $ base64Encode <$> BS.readFile image
  validateImage isDashboard (personId, merchantId, merchantOpCityId) $ ImageValidateRequest image' imageType

mkImage ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Id DM.Merchant ->
  Text ->
  Domain.ImageType ->
  Bool ->
  m Domain.Image
mkImage personId_ merchantId s3Path imageType_ isValid = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Image
      { id,
        personId = personId_,
        merchantId,
        s3Path,
        imageType = imageType_,
        isValid,
        failureReason = Nothing,
        createdAt = now
      }

getImage :: Id DM.Merchant -> Id Domain.Image -> Flow Text
getImage merchantId imageId = do
  imageMetadata <- Query.findById imageId
  case imageMetadata of
    Just img | img.merchantId == merchantId -> S3.get $ T.unpack img.s3Path
    _ -> pure T.empty
