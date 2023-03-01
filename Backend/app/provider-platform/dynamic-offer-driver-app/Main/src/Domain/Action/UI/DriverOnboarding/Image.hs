{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Image where

import AWS.S3 as S3
import qualified Data.ByteString as BS
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Person as Person
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import qualified Idfy.Flow as Idfy
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (isNothing)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Multipart
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverOnboarding.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.Person as Person

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

getDocType :: Domain.ImageType -> Text
getDocType Domain.DriverLicense = "ind_driving_license"
getDocType Domain.VehicleRegistrationCertificate = "ind_rc"

getImageType :: Text -> Domain.ImageType
getImageType "ind_driving_license" = Domain.DriverLicense
getImageType "ind_rc" = Domain.VehicleRegistrationCertificate
getImageType _ = Domain.VehicleRegistrationCertificate

validateImage ::
  Bool ->
  Maybe Merchant ->
  Id Person.Person ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage isDashboard mbMerchant personId ImageValidateRequest {..} = do
  person <- Person.findById (Proxy @Flow) personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantId = person.merchantId

  org <- case mbMerchant of
    Nothing -> do
      CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    Just merchant -> do
      -- merchant access checking
      unless (merchant.id == merchantId) $ throwError (PersonNotFound personId.getId)
      pure merchant

  images <- Query.findRecentByPersonIdAndImageType personId imageType (Proxy @Flow)
  unless isDashboard $ do
    onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
    when (length images > onboardingTryLimit) $ do
      driverPhone <- mapM decrypt person.mobileNumber
      notifyErrorToSupport driverPhone org.name ((.failureReason) <$> images)
      throwError (ImageValidationExceedLimit personId.getId)

  imagePath <- createPath personId.getId merchantId.getId imageType
  _ <- fork "S3 Put Image" $ S3.put (T.unpack imagePath) image
  imageEntity <- mkImage personId merchantId imagePath imageType False
  runTransaction $ Query.create @Flow imageEntity

  -- skipping validation for rc as validation not available in idfy
  unless (imageType == Domain.VehicleRegistrationCertificate) $ do
    validationOutput <- Idfy.validateImage image (getDocType imageType)
    checkErrors imageEntity.id imageType validationOutput.result
  runTransaction $ Query.updateToValid @Flow imageEntity.id

  return $ ImageValidateResponse {imageId = imageEntity.id}
  where
    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just result) = do
      let outputImageType = getImageType result.detected_doc_type
      unless (outputImageType == imgType) $ throwImageError id_ (ImageInvalidType (show imgType) (show outputImageType))

      unless (fromMaybe False result.is_readable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) result.readability.confidence) $
        throwImageError id_ ImageLowQuality

validateImageFile ::
  Bool ->
  Id Person.Person ->
  ImageValidateFileRequest ->
  Flow ImageValidateResponse
validateImageFile isDashboard personId ImageValidateFileRequest {..} = do
  image' <- L.runIO $ base64Encode <$> BS.readFile image
  validateImage isDashboard Nothing personId $ ImageValidateRequest image' imageType

mkImage ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Id Merchant ->
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

-- FIXME: Temporary API will move to dashboard later
getDocs :: Person.Person -> Text -> Flow GetDocsResponse
getDocs _admin mobileNumber = do
  mobileNumberHash <- getDbHash mobileNumber
  driver <- runInReplica $ Person.findByMobileNumber "+91" mobileNumberHash (Proxy @Flow) >>= fromMaybeM (PersonDoesNotExist mobileNumber)
  let merchantId = driver.merchantId
  dl <- runInReplica $ DLQuery.findByDriverId driver.id (Proxy @Flow)
  let dlImageId = (.documentImageId1) <$> dl

  association <- DRAQuery.getActiveAssociationByDriver driver.id (Proxy @Flow)
  rc <-
    maybe
      (pure Nothing)
      (\assoc_ -> runInReplica $ RCQuery.findById (Proxy @Flow) assoc_.rcId)
      association
  let rcImageId = (.documentImageId) <$> rc

  dlImage <- getImage merchantId `mapM` dlImageId
  rcImage <- getImage merchantId `mapM` rcImageId

  return GetDocsResponse {dlImage, rcImage}

getImage :: Id Merchant -> Id Domain.Image -> Flow Text
getImage merchantId imageId = do
  imageMetadata <- Query.findById (Proxy @Flow) imageId
  case imageMetadata of
    Just img | img.merchantId == merchantId -> S3.get $ T.unpack img.s3Path
    _ -> pure T.empty
