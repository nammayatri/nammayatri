{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Image where

import AWS.S3 as S3
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Text as T hiding (length)
import Data.Time.Format.ISO8601
import qualified Domain.Types.DriverOnboarding.Image as Domain
import qualified Domain.Types.Person as Person
import qualified Storage.Queries.DriverOnboarding.Image as Query
import qualified Storage.Queries.Person as Person
import Tools.Error
import Tools.Metrics

data ValidateImageReq = ValidateImageReq
  { image :: Text,
    imageType :: Domain.ImageType
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype ValidateImageRes = ValidateImageRes
  {imageId :: Id Domain.Image}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

createPath ::
  (HasFlowEnv m r '["s3Config" ::: S3.S3Config]) =>
  Text ->
  Text ->
  Domain.ImageType ->
  m Text
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

validateImage :: 
  ( CoreMetrics m,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["s3Config" ::: S3Config]
  ) =>
  Id Person.Person -> 
  ValidateImageReq -> 
  m ValidateImageRes
validateImage personId ValidateImageReq {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")

  images <- Query.findByPersonId personId
  when (length images < 3) $ throwError (ImageValidationExceedLimit personId.getId)

  imagePath <- createPath personId.getId orgId.getId imageType
  _ <- S3.put (T.unpack imagePath) image
  imageEntity <- mkImage personId orgId imagePath imageType False
  runTransaction $ Query.create imageEntity

  -- Idfy logic :: Uncomment this once Idfy service is ready
  -- validationOutput <- Idfy.validateImage image >>= throwError InvalidImage
  -- unless (validationOutput.docType == imageType) $ throwError (InvalidImageType imageType validationOutput.docType)
  -- runTransaction $ Query.updateToValid imageEntity.id

  return $ ValidateImageRes {imageId = imageEntity.id}
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
