{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.DriverLicense
  ( DriverDLReq (..),
    DriverDLRes,
    verifyDL,
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Time (nominalDay)
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.DriverOnboarding.Image as Image
import qualified Domain.Types.Person as Person
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.DriverOnboarding.Image as ImageQuery
import qualified Storage.Queries.Person as Person
import Tools.Error

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime,
    imageId :: Id Image.Image
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type DriverDLRes = APISuccess

validateDriverDLReq :: UTCTime -> Validate DriverDLReq
validateDriverDLReq now DriverDLReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber licenseNum,
      validateField "driverDateOfBirth" driverDateOfBirth $ InRange @UTCTime t100YearsAgo t16YearsAgo
    ]
  where
    licenseNum = MinLength 5 `And` star (latinUC \/ digit)
    t16YearsAgo = yearsAgo 16
    t100YearsAgo = yearsAgo 100
    yearsAgo i = negate (nominalDay * 365 * i) `addUTCTime` now

verifyDL ::
  ( EsqDBFlow m r,
    EncFlow m r
  ) =>
  Id Person.Person -> 
  DriverDLReq -> 
  m DriverDLRes
verifyDL personId req@DriverDLReq {..} = do
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  imageMetadata <- ImageQuery.findById imageId >>= fromMaybeM (ImageNotFound imageId.getId)
  unless (imageMetadata.isValid) $ throwError (ImageNotValid imageId.getId)
  unless (imageMetadata.imageType == Image.DriverLicense) $
    throwError (InvalidImageType (show Image.DriverLicense) (show imageMetadata.imageType))

  -- Uncomment this once Idfy service is ready
  -- image <- S3.get imageMetadata.s3Path
  -- extractOutput <- Idfy.extractImage image
  -- unless (extractOutput.certificateNumber == vehicleRegistrationCertNumber) $
  --   throwError (ImageDataNotMatching extractOutput.certificateNumber vehicleRegistrationCertNumber)

  mdriverLicense <- Query.findActiveDL driverLicenseNumber

  case mdriverLicense of
    Just driverLicense -> do
      when (driverLicense.driverId == personId) $ throwError AlreadyRegisteredSamePerson
      throwError AlreadyRegisteredDiffPerson
    Nothing -> do
      mlatestDriverDL <- Query.findLatestByPersonId personId
      latestVersion <-
        maybe
          (pure 1)
          ( \latestDriverDL -> do
              runTransaction $ Query.makeDLInactive latestDriverDL.id now
              pure $ latestDriverDL.version + 1
          )
          mlatestDriverDL

      rcEntity <- mkDriverLicenseEntry personId (Just driverDateOfBirth) driverLicenseNumber Nothing now latestVersion
      runTransaction $ Query.create rcEntity

  return Success

mkDriverLicenseEntry :: EncFlow m r => Id Person.Person -> Maybe UTCTime -> Text -> Maybe Text -> UTCTime -> Int -> m Domain.DriverLicense
mkDriverLicenseEntry personId dob dlNumber reqId time version = do
  ddl <- encrypt dlNumber
  id <- generateGUID
  return $
    Domain.DriverLicense
      { id,
        driverId = personId,
        driverDob = dob,
        licenseNumber = ddl,
        licenseStart = Nothing,
        licenseExpiry = Nothing,
        classOfVehicles = [],
        verificationStatus = Domain.PENDING,
        idfyRequestId = reqId,
        idfyResponseDump = Nothing,
        version,
        active = True,
        consent = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time
      }
