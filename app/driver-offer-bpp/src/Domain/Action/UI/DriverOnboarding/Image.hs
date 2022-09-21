{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Image where

import AWS.S3 as S3
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Person as Person
import Environment
import Idfy.Flow as Idfy
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Storage.Queries.Person as Person
import Tools.Error

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
      ( "/" <> T.unpack pathPrefix <> "/driver-onboarding/" <> "org-" <> T.unpack orgId <> "/"
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

  images <- Query.findByPersonId personId
  when (length images > 100) $ throwError (ImageValidationExceedLimit personId.getId)

  imagePath <- createPath personId.getId orgId.getId imageType
  _ <- S3.put (T.unpack imagePath) image
  imageEntity <- mkImage personId orgId imagePath imageType False
  runTransaction $ Query.create imageEntity

  validationOutput <- Idfy.validateImage image (getDocType imageType)
  checkErrors imageType validationOutput.result
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
            createdAt = now
          }

    checkErrors _ Nothing = throwError InvalidImage
    checkErrors imgType (Just result) = do
      let outputImageType = getImageType result.detected_doc_type
      unless (outputImageType == imgType) $ throwError (InvalidImageType (show imgType) (show outputImageType))

      unless (fromMaybe False result.is_readable) $ throwError $ InvalidRequest "Image not readable"

      unless (maybe False (60 <) result.readability.confidence) $
        throwError $ InvalidRequest "Image quality is not good"
