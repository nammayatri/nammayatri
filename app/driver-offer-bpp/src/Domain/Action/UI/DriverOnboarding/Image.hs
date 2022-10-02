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
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Person as Person
import Environment
import Idfy.Flow as Idfy
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person

data ImageValidateRequest = ImageValidateRequest
  { image :: Text,
    imageType :: Domain.ImageType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype ImageValidateResponse = ImageValidateResponse
  {imageId :: Id Domain.Image}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

createPath ::
  Text ->
  Text ->
  Domain.ImageType ->
  Flow Text
createPath driverId orgId imageType = do
  S3Config {..} <- asks (.s3Config)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return $
    T.pack
      ( T.unpack pathPrefix <> "/driver-onboarding/" <> "org-" <> T.unpack orgId <> "/"
          <> T.unpack driverId
          <> "/"
          <> show imageType
          <> "/"
          <> T.unpack fileName
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
  Id Person.Person ->
  ImageValidateRequest ->
  Flow ImageValidateResponse
validateImage personId ImageValidateRequest {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  org <- Organization.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)

  images <- Query.findRecentByPersonIdAndImageType personId imageType
  onboardingTryLimit <- asks (.onboardingTryLimit)
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

    checkErrors id_ _ Nothing = throwImageError id_ ImageValidationFailed
    checkErrors id_ imgType (Just result) = do
      let outputImageType = getImageType result.detected_doc_type
      unless (outputImageType == imgType) $ throwImageError id_ (ImageInvalidType (show imgType) (show outputImageType))

      unless (fromMaybe False result.is_readable) $ throwImageError id_ ImageNotReadable

      unless (maybe False (60 <) result.readability.confidence) $
        throwImageError id_ ImageLowQuality
