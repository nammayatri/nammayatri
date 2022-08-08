{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Product.DriverOnboarding.VehicleRegistrationCertificate where

import AWS.S3 as S3
import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Error
import Beckn.Types.Id
import Data.Text as T
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as Domain
import qualified Domain.Types.DriverOnboarding.VehicleRegistrationCertificate as Domain
import qualified Domain.Types.Person as Person
import Environment
import SharedLogic.DriverOnboarding
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as Query
import qualified Storage.Queries.Person as Person
import qualified Types.API.DriverOnboarding.VehicleRegistrationCertificate as API
import Utils.Common

validateRCImage :: Id Person.Person -> API.DriverRCImageReq -> FlowHandler API.DriverRCRes
validateRCImage personId API.DriverRCImageReq {..} = withFlowHandlerAPI $ do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  mVehicleRC <- Query.findByPersonId personId
  imagePath <- createPath personId.getId orgId.getId "rc"
  now <- getCurrentTime

  case mVehicleRC of
    Nothing -> do
      _ <- S3.put (T.unpack imagePath) image
      rcEntity <- mkVehicleRegistrationCertEntry personId "req.certificateNumber" Nothing now imagePath
      runTransaction $ Query.create rcEntity
    Just _ -> do
      _ <- S3.put (T.unpack imagePath) image
      runTransaction $ Query.updateImagePath personId imagePath

  return Success

verifyRC :: Id Person.Person -> API.DriverRCReq -> FlowHandler API.DriverRCRes
verifyRC personId req@API.DriverRCReq {..} = withFlowHandlerAPI $ do
  _ <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  mVehicleRC <- Query.findByPersonId personId
  now <- getCurrentTime
  case mVehicleRC of
    Nothing -> do
      rcEntity <- mkVehicleRegistrationCertEntry personId req.vehicleRegistrationCertNumber Nothing now T.empty
      runTransaction $ Query.create rcEntity
    Just vehicleRC -> do
      rcNumber <- decrypt vehicleRC.certificateNumber
      when (rcNumber /= req.vehicleRegistrationCertNumber) do
        runTransaction $ Query.resetRCRequest personId req.vehicleRegistrationCertNumber Nothing now
  return Success

mkVehicleRegistrationCertEntry :: EncFlow m r => Id Person.Person -> Text -> Maybe Text -> UTCTime -> Text -> m Domain.VehicleRegistrationCertificate
mkVehicleRegistrationCertEntry personId rcNumber reqID time path = do
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
