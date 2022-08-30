{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
  ( DriverRCReq (..),
    DriverRCRes,
    verifyRC,
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Person as Person
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as Query
import qualified Storage.Queries.Person as Person
import Tools.Error

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Id Image.Image,
    operatingCity :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverRCRes = APISuccess

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

verifyRC ::
  ( EsqDBFlow m r,
    EncFlow m r
  ) =>
  Id Person.Person -> 
  DriverRCReq -> 
  m DriverRCRes
verifyRC personId req@DriverRCReq {..} = do
  runRequestValidation validateDriverRCReq req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
  unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
  unless (imageMetadata.imageType == Image.VehicleRegistrationCertificate) $
    throwError (InvalidImageType (show Image.VehicleRegistrationCertificate) (show imageMetadata.imageType))

  -- Uncomment this once Idfy service is ready
  -- image <- S3.get imageMetadata.s3Path
  -- extractOutput <- Idfy.extractImage image
  -- unless (extractOutput.certificateNumber == vehicleRegistrationCertNumber) $
  --   throwError (ImageDataNotMatching extractOutput.certificateNumber vehicleRegistrationCertNumber)

  mVehicleRC <- Query.findActiveVehicleRC vehicleRegistrationCertNumber

  case mVehicleRC of
    Just vehicleRC -> do
      when (vehicleRC.driverId == personId) $ throwError AlreadyRegisteredSamePerson
      throwError AlreadyRegisteredDiffPerson
    Nothing -> do
      now <- getCurrentTime
      mlatestDriverRC <- Query.findLatestByPersonId personId
      latestVersion <-
        maybe
          (pure 1)
          ( \latestDriverRC -> do
              runTransaction $ Query.makeRCInactive latestDriverRC.id now
              pure $ latestDriverRC.version + 1
          )
          mlatestDriverRC

      rcEntity <- mkVehicleRegistrationCertEntry personId req.vehicleRegistrationCertNumber Nothing now latestVersion
      runTransaction $ Query.create rcEntity

  return Success

mkVehicleRegistrationCertEntry :: EncFlow m r => Id Person.Person -> Text -> Maybe Text -> UTCTime -> Int -> m Domain.VehicleRegistrationCertificate
mkVehicleRegistrationCertEntry personId rcNumber reqID time version = do
  vrc <- encrypt rcNumber
  id <- generateGUID
  return $
    Domain.VehicleRegistrationCertificate
      { id,
        driverId = personId,
        certificateNumber = vrc,
        fitnessExpiry = Nothing,
        permitNumber = Nothing,
        permitStart = Nothing,
        permitExpiry = Nothing,
        pucExpiry = Nothing,
        vehicleColor = Nothing,
        vehicleManufacturer = Nothing,
        vehicleModel = Nothing,
        vehicleClass = Nothing,
        idfyRequestId = reqID,
        idfyResponseDump = Nothing,
        insuranceValidity = Nothing,
        verificationStatus = Domain.PENDING,
        version,
        active = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time,
        consent = True
      }
