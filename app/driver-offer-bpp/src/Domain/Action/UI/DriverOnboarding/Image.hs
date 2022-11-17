{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Image where

import AWS.S3 as S3
import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Data.ByteString as BS
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import Domain.Types.Organization
import qualified Domain.Types.Person as Person
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import qualified Idfy.Flow as Idfy
import Servant.Multipart
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.Organization as Organization
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

data ImageValidateFormDataRequest = ImageValidateFormDataRequest
  { image :: FilePath,
    imageType :: Domain.ImageType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

instance FromMultipart Tmp ImageValidateFormDataRequest where
  fromMultipart form = do
    ImageValidateFormDataRequest
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
createPath driverId orgId imageType = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/driver-onboarding/" <> "org-" <> orgId <> "/"
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
  Id Person.Person ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage isDashboard personId ImageValidateRequest {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  org <- Organization.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)

  images <- Query.findRecentByPersonIdAndImageType personId imageType
  unless isDashboard $ do
    onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
    when (length images > onboardingTryLimit) $ do
      driverPhone <- mapM decrypt person.mobileNumber
      notifyErrorToSupport driverPhone org.name ((.failureReason) <$> images)
      throwError (ImageValidationExceedLimit personId.getId)

  imagePath <- createPath personId.getId orgId.getId imageType
  _ <- S3.put (T.unpack imagePath) image
  imageEntity <- mkImage personId orgId imagePath imageType False
  runTransaction $ Query.create imageEntity

  -- skipping validation for rc as validation not available in idfy
  unless (imageType == Domain.VehicleRegistrationCertificate) $ do
    validationOutput <- Idfy.validateImage image (getDocType imageType)
    checkErrors imageEntity.id imageType validationOutput.result
  runTransaction $ Query.updateToValid imageEntity.id

  return $ ImageValidateResponse {imageId = imageEntity.id}
  where
    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just result) = do
      let outputImageType = getImageType result.detected_doc_type
      unless (outputImageType == imgType) $ throwImageError id_ (ImageInvalidType (show imgType) (show outputImageType))

      unless (fromMaybe False result.is_readable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) result.readability.confidence) $
        throwImageError id_ ImageLowQuality

validateImageFormData ::
  Bool ->
  Id Person.Person ->
  ImageValidateFormDataRequest ->
  Flow ImageValidateResponse
validateImageFormData isDashboard personId ImageValidateFormDataRequest {..} = do
  image' <- L.runIO $ base64Encode <$> BS.readFile image
  validateImage isDashboard personId $ ImageValidateRequest image' imageType

mkImage ::
  (MonadGuid m, MonadTime m) =>
  Id Person.Person ->
  Id Organization ->
  Text ->
  Domain.ImageType ->
  Bool ->
  m Domain.Image
mkImage personId_ organizationId s3Path imageType_ isValid = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Image
      { id,
        personId = personId_,
        organizationId,
        s3Path,
        imageType = imageType_,
        isValid,
        failureReason = Nothing,
        createdAt = now
      }

-- FIXME: Temporary API will move to dashboard later
getDocs :: Person.Person -> Text -> Flow GetDocsResponse
getDocs _admin mobileNumber = do
  driver <- Person.findByMobileNumber "+91" mobileNumber >>= fromMaybeM (PersonDoesNotExist mobileNumber)

  dl <- DLQuery.findByDriverId driver.id
  let dlImageId = (.documentImageId1) <$> dl

  association <- DRAQuery.getActiveAssociationByDriver driver.id
  rc <-
    maybe
      (pure Nothing)
      (\assoc_ -> RCQuery.findById assoc_.rcId)
      association
  let rcImageId = (.documentImageId) <$> rc

  dlImage <- getImage `mapM` dlImageId
  rcImage <- getImage `mapM` rcImageId

  return GetDocsResponse {dlImage, rcImage}

getImage :: Id Domain.Image -> Flow Text
getImage imageId = do
  imageMetadata <- Query.findById imageId
  maybe
    (pure T.empty)
    (\img -> S3.get $ T.unpack img.s3Path)
    imageMetadata
