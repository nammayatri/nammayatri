{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Product.DriveronBoarding.DriverOnBoarding where
import Environment

import Domain.Types.Driveronboarding.OperatingCity as DOC
import Beckn.Types.Id
import qualified EulerHS.Language as L
import Beckn.Storage.Esqueleto hiding (isNothing)
import Utils.Common
import Prelude
import Beckn.Prelude
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert  as QVR
import Types.API.Driveronboarding.DriverOnBoarding
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as DVR hiding (VALID)
import qualified Domain.Types.Driveronboarding.DriverDrivingLicense as DDL
import qualified Domain.Types.Organization as DO
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import Beckn.Types.APISuccess
import Beckn.External.Encryption
import Types.Error
import Beckn.Types.Validation
import Beckn.Utils.Validation
import Beckn.Types.Predicate
import Beckn.Utils.Predicates

validateDriverOnBoarding :: Validate DriverOnBoardingReq
validateDriverOnBoarding DriverOnBoardingReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" driverLicenseNumber $ MinLength 5 `And` text
    ]
    where
      extractMaybe (Just x) = x
      text = star $ alphanum \/  ","

registrationHandler :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler personId req@DriverOnBoardingReq {..}= withFlowHandlerAPI $ do
  if driverConsent then do
    runRequestValidation validateDriverOnBoarding DriverOnBoardingReq {..}
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    let orgId = Id req.organizationId :: Id DO.Organization
    organization <- QOrganization.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
    let orgTxt = getId organization.id
    task_id <- L.generateGUID -- task_id for idfy request
    let group_id = personId   -- group_id for idfy request
    now <- getCurrentTime
    driverDLDetails <- QDDL.findByDId personId
    vehicleRCDetails <- QVR.findByPId personId
    handleDLVerification req personId driverDLDetails
    handleRCVerification req personId vehicleRCDetails
    return Success 
  else 
    throwError (InvalidRequest "User Consent is required")

handleDLVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DDL.DriverDrivingLicense -> Flow ()
handleDLVerification req personId dl= do
  now <- getCurrentTime
  case dl of
    Nothing -> do
      dlId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      dlEntity <- mkDriverDrivingLicenseEntry dlId personId (Just req.driverDateOfBirth) (Just req.driverLicenseNumber) idfyReqId now
      runTransaction $ QDDL.create dlEntity
      return ()
    Just dlRecord -> do
      dlNumber <- mapM decrypt dlRecord.driverLicenseNumber
      when (dlNumber /= Just req.driverLicenseNumber || dlRecord.driverDob /= Just req.driverDateOfBirth) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QDDL.resetDLRequest personId (Just req.driverLicenseNumber) (Just req.driverDateOfBirth) idfyReqId now


handleRCVerification ::  DriverOnBoardingReq -> Id SP.Person -> Maybe DVR.VehicleRegistrationCert -> Flow ()
handleRCVerification req personId rc = do
  now <- getCurrentTime
  case rc of
    Nothing -> do
      rcId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      rcEntity <- mkVehicleRegistrationCertEntry rcId personId (Just req.vehicleRegistrationCertNumber) idfyReqId now
      runTransaction $ QVR.create rcEntity
      return ()
    Just rcRecord -> do
      rcNumber <- mapM decrypt rcRecord.vehicleRegistrationCertNumber
      when (rcNumber /= Just req.vehicleRegistrationCertNumber) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QVR.resetRCRequest personId (Just req.driverLicenseNumber) idfyReqId now

        


mkOperatingCityEntry :: Text -> Text -> DriverOnBoardingReq -> UTCTime -> DOC.OperatingCity
mkOperatingCityEntry opId orgId req time =
  DOC.OperatingCity
  {
    id = Id opId,
    organizationId = Id orgId,
    cityName = req.operatingCity,
    enabled = VALID,
    createdAt = time,
    updatedAt = time
  }

mkVehicleRegistrationCertEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe Text -> Text -> UTCTime -> m DVR.VehicleRegistrationCert
mkVehicleRegistrationCertEntry opId personId rcNumber reqID time = do
  vrc <- mapM encrypt rcNumber
  return $
    DVR.VehicleRegistrationCert
    {
      id = Id opId,
      driverId = personId,
      vehicleRegistrationCertNumber = vrc,
      fitnessCertExpiry = Nothing,
      permitNumber = Nothing,
      permitStart = Nothing,
      permitExpiry = Nothing,
      vehicleClass = Nothing,
      request_id = reqID,
      vehicleNumber = Nothing,
      insuranceValidity = Nothing,
      idfyStatus = DVR.IN_PROGRESS,
      verificationStatus = DVR.PENDING,
      createdAt = time,
      updatedAt = time,
      consentTimestamp = time,
      consent = True
    }


mkDriverDrivingLicenseEntry :: EncFlow m r => Text -> Id SP.Person -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime ->m DDL.DriverDrivingLicense
mkDriverDrivingLicenseEntry opId personId dob dlNumber reqId time = do
  ddl <- mapM encrypt dlNumber
  return $
    DDL.DriverDrivingLicense
      {
        id = Id opId,
        driverId = personId,
        driverDob = dob,
        driverLicenseNumber = ddl,
        driverLicenseStart = Nothing,
        driverLicenseExpiry = Nothing,
        classOfVehicle = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        driverVerificationStatus = DVR.PENDING,
        request_id = reqId,
        consent = True,
        createdAt = time,
        updatedAt = time,
        consentTimestamp = time
      }

