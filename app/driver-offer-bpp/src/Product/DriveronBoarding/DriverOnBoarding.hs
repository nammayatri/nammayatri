{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Product.DriveronBoarding.DriverOnBoarding where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Validation
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as DDL
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as DVR hiding (VALID)
import qualified Domain.Types.Person as SP
import Environment
import qualified EulerHS.Language as L
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as QVR
import Types.API.Driveronboarding.DriverOnBoarding
import Utils.Common

registrationHandler :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler personId req@DriverOnBoardingReq {..} = withFlowHandlerAPI $ do
  now <- getCurrentTime
  runRequestValidation (validateDriverOnBoardingReq now) DriverOnBoardingReq {..}
  driverDLDetails <- QDDL.findByDId personId
  vehicleRCDetails <- QVR.findByPId personId
  handleDLVerification req personId driverDLDetails
  handleRCVerification req personId vehicleRCDetails
  return Success

handleDLVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DDL.DriverDrivingLicense -> Flow ()
handleDLVerification req personId dl = do
  now <- getCurrentTime
  case dl of
    Nothing -> do
      dlId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      let image = "img" :: Text
      let result = "res" :: Text
      dlEntity <- mkDriverDrivingLicenseEntry dlId personId (Just req.driverDateOfBirth) (Just req.driverLicenseNumber) idfyReqId image result now
      runTransaction $ QDDL.create dlEntity
      return ()
    Just dlRecord -> do
      dlNumber <- mapM decrypt dlRecord.driverLicenseNumber
      when (dlNumber /= Just req.driverLicenseNumber || dlRecord.driverDob /= Just req.driverDateOfBirth) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QDDL.resetDLRequest personId (Just req.driverLicenseNumber) (Just req.driverDateOfBirth) idfyReqId now

handleRCVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DVR.VehicleRegistrationCert -> Flow ()
handleRCVerification req personId rc = do
  now <- getCurrentTime
  case rc of
    Nothing -> do
      rcId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      let image = "img" :: Text
      let result = "res" :: Text
      rcEntity <- mkVehicleRegistrationCertEntry rcId personId (Just req.vehicleRegistrationCertNumber) idfyReqId image result now
      runTransaction $ QVR.create rcEntity
      return ()
    Just rcRecord -> do
      rcNumber <- mapM decrypt rcRecord.vehicleRegistrationCertNumber
      when (rcNumber /= Just req.vehicleRegistrationCertNumber) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QVR.resetRCRequest personId (Just req.driverLicenseNumber) idfyReqId now

mkVehicleRegistrationCertEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe Text -> Text -> Text ->Text-> UTCTime -> m DVR.VehicleRegistrationCert
mkVehicleRegistrationCertEntry opId personId rcNumber  reqID img res time = do
  vrc <- mapM encrypt rcNumber
  return $
    DVR.VehicleRegistrationCert
      { id = Id opId,
        driverId = personId,
        vehicleRegistrationCertNumber = vrc,
        fitnessCertExpiry = Nothing,
        permitNumber = Nothing,
        permitStart = Nothing,
        permitExpiry = Nothing,
        pucExpiry = Nothing,
        vehicleClass = Nothing,
        vehicleColor = Nothing,
        vehicleManufacturer = Nothing,
        vehicleModel = Nothing,
        rcImageS3Path = img,
        idfyRequestId = reqID,
        vehicleNumber = Nothing,
        insuranceValidity = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        verificationRespDump = res,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time,
        consent = True
      }

mkDriverDrivingLicenseEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe UTCTime -> Maybe Text  -> Text -> Text -> Text ->UTCTime -> m DDL.DriverDrivingLicense
mkDriverDrivingLicenseEntry opId personId dob dlNumber reqId img res time = do
  ddl <- mapM encrypt dlNumber
  return $
    DDL.DriverDrivingLicense
      { id = Id opId,
        driverId = personId,
        driverDob = dob,
        driverLicenseNumber = ddl,
        driverLicenseStart = Nothing,
        driverLicenseExpiry = Nothing,
        classOfVehicle = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        verificationRespDump = res,
        dlImage1S3Path = img,
        dlImage2S3Path = img,
        idfyRequestId= reqId,
        consent = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time
      }
