{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.DriverLicense
  ( DriverDLImageReq (..),
    DriverDLReq (..),
    DriverDLRes,
    validateDLImage,
    verifyDL,
  )
where

import AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Types.Validation
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Text as T
import Data.Time (nominalDay)
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.Person as Person
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.Person as Person
import Tools.Metrics
import Utils.Common

newtype DriverDLImageReq = DriverDLImageReq
  {image :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data DriverDLReq = DriverDLReq
  { driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

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

type DriverDLRes = APISuccess

validateDLImage ::
  ( EncFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["s3Config" ::: S3Config],
    CoreMetrics m
  ) =>
  Id Person.Person ->
  DriverDLImageReq ->
  m DriverDLRes
validateDLImage personId DriverDLImageReq {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  mdriverLicense <- Query.findByDriverId personId
  imagePath <- createPath personId.getId orgId.getId "dl"
  now <- getCurrentTime

  case mdriverLicense of
    Nothing -> do
      _ <- S3.put (T.unpack imagePath) image
      rcEntity <- buildDriverLicenseEntry personId Nothing "dummy" Nothing now imagePath
      runTransaction $ Query.create rcEntity
    Just _ -> do
      _ <- S3.put (T.unpack imagePath) image
      runTransaction $ Query.updateImagePath personId imagePath

  return Success

verifyDL ::
  ( EncFlow m r,
    EsqDBFlow m r
  ) =>
  Id Person.Person ->
  DriverDLReq ->
  m DriverDLRes
verifyDL personId req@DriverDLReq {..} = do
  now <- getCurrentTime
  runRequestValidation (validateDriverDLReq now) req
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  mdriverLicense <- Query.findByDriverId personId
  case mdriverLicense of
    Nothing -> do
      rcEntity <- buildDriverLicenseEntry personId (Just req.driverDateOfBirth) req.driverLicenseNumber Nothing now T.empty
      runTransaction $ Query.create rcEntity
    Just driverLicense -> do
      dlNumber <- decrypt driverLicense.licenseNumber
      when (dlNumber /= req.driverLicenseNumber) do
        runTransaction $ Query.resetDLRequest personId req.driverLicenseNumber (Just req.driverDateOfBirth) Nothing now
  return Success

buildDriverLicenseEntry :: EncFlow m r => Id Person.Person -> Maybe UTCTime -> Text -> Maybe Text -> UTCTime -> Text -> m Domain.DriverLicense
buildDriverLicenseEntry personId dob dlNumber reqId time path = do
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
        verificationTryCount = 0,
        imageS3Path = path,
        consent = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time
      }
