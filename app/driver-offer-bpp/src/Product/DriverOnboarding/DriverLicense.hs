{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Product.DriverOnboarding.DriverLicense where

import AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Data.Text as T
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.DriverLicense as Domain
import qualified Domain.Types.Person as Person
import Environment
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.DriverLicense as Query
import qualified Storage.Queries.Person as Person
import qualified Types.API.DriverOnboarding.DriverLicense as API
import Utils.Common

validateDLImage :: Id Person.Person -> API.DriverDLImageReq -> FlowHandler API.DriverDLRes
validateDLImage personId API.DriverDLImageReq {..} = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  mdriverLicense <- Query.findByDriverId personId
  imagePath <- createPath personId.getId orgId.getId "dl"
  now <- getCurrentTime

  case mdriverLicense of
    Nothing -> do
      _ <- S3.put (T.unpack imagePath) image
      rcEntity <- mkDriverLicenseEntry personId Nothing "dummy" Nothing now imagePath
      runTransaction $ Query.create rcEntity
    Just _ -> do
      _ <- S3.put (T.unpack imagePath) image
      runTransaction $ Query.updateImagePath personId imagePath

  return Success

verifyDL :: Id Person.Person -> API.DriverDLReq -> FlowHandler API.DriverDLRes
verifyDL personId req@API.DriverDLReq {..} = withFlowHandlerAPI $ do
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  mdriverLicense <- Query.findByDriverId personId
  now <- getCurrentTime
  case mdriverLicense of
    Nothing -> do
      rcEntity <- mkDriverLicenseEntry personId (Just req.driverDateOfBirth) req.driverLicenseNumber Nothing now T.empty
      runTransaction $ Query.create rcEntity
    Just driverLicense -> do
      dlNumber <- decrypt driverLicense.licenseNumber
      when (dlNumber /= req.driverLicenseNumber) do
        runTransaction $ Query.resetDLRequest personId req.driverLicenseNumber (Just req.driverDateOfBirth) Nothing now
  return Success

mkDriverLicenseEntry :: EncFlow m r => Id Person.Person -> Maybe UTCTime -> Text -> Maybe Text -> UTCTime -> Text -> m Domain.DriverLicense
mkDriverLicenseEntry personId dob dlNumber reqId time path = do
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
