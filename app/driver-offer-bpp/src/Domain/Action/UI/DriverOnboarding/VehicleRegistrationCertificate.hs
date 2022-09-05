{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate
  ( DriverRCImageReq (..),
    DriverRCReq (..),
    DriverRCRes,
    validateRCImage,
    verifyRC,
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
import Beckn.Utils.Common
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.Text as T
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Person as Person
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as Query
import qualified Storage.Queries.Person as Person
import Tools.Metrics

newtype DriverRCImageReq = DriverRCImageReq
  {image :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data DriverRCReq = DriverRCReq
  { vehicleRegistrationCertNumber :: Text,
    operatingCity :: Text
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

validateDriverRCReq :: Validate DriverRCReq
validateDriverRCReq DriverRCReq {..} =
  sequenceA_
    [validateField "vehicleRegistrationCertNumber" vehicleRegistrationCertNumber certNum]
  where
    certNum = LengthInRange 5 12 `And` star (latinUC \/ digit \/ ",")

type DriverRCRes = APISuccess

validateRCImage ::
  ( EsqDBFlow m r,
    HasFlowEnv m r '["s3Config" ::: S3Config],
    EncFlow m r,
    CoreMetrics m
  ) =>
  Id Person.Person ->
  DriverRCImageReq ->
  m DriverRCRes
validateRCImage personId DriverRCImageReq {..} = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  mVehicleRC <- Query.findByPersonId personId
  imagePath <- createPath personId.getId orgId.getId "rc"
  now <- getCurrentTime

  case mVehicleRC of
    Nothing -> do
      _ <- S3.put (T.unpack imagePath) image
      rcEntity <- buildVehicleRegistrationCertEntry personId "req.certificateNumber" Nothing now imagePath
      runTransaction $ Query.create rcEntity
    Just _ -> do
      _ <- S3.put (T.unpack imagePath) image
      runTransaction $ Query.updateImagePath personId imagePath

  return Success

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

  mVehicleRC <- Query.findByPersonId personId
  now <- getCurrentTime
  case mVehicleRC of
    Nothing -> do
      rcEntity <- buildVehicleRegistrationCertEntry personId req.vehicleRegistrationCertNumber Nothing now T.empty
      runTransaction $ Query.create rcEntity
    Just vehicleRC -> do
      rcNumber <- decrypt vehicleRC.certificateNumber
      when (rcNumber /= req.vehicleRegistrationCertNumber) do
        runTransaction $ Query.resetRCRequest personId req.vehicleRegistrationCertNumber Nothing now
  return Success

buildVehicleRegistrationCertEntry :: EncFlow m r => Id Person.Person -> Text -> Maybe Text -> UTCTime -> Text -> m Domain.VehicleRegistrationCertificate
buildVehicleRegistrationCertEntry personId rcNumber reqID time path = do
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
        imageS3Path = path,
        idfyResponseDump = Nothing,
        insuranceValidity = Nothing,
        verificationTryCount = 0,
        verificationStatus = Domain.PENDING,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time,
        consent = True
      }
