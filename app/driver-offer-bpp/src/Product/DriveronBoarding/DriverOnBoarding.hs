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

validateIssue1 :: Validate DriverOnBoardingReq
validateIssue1 DriverOnBoardingReq {..} =
  sequenceA_
    [ validateField "driverLicenseNumber" (extractMaybe driverLicenseNumber) $ MinLength 5 `And` text
    ]
    where
      extractMaybe (Just x) = x
      text = star $ alphanum \/  ","

registrationHandler1 :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler1 personId req@DriverOnBoardingReq {..}= withFlowHandlerAPI $ do
  runRequestValidation validateIssue1 DriverOnBoardingReq {..}
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let orgId = Id req.organizationId :: Id DO.Organization
  organization <- QOrganization.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  let orgTxt = getId organization.id
  task_id <- L.generateGUID -- task_id for idfy request
  let group_id = personId   -- group_id for idfy request
  now <- getCurrentTime
  dlDetails <- QDDL.findByDId personId
  rcDetails <- QVR.findByPId personId
  handleDLVerification req personId dlDetails
  handleRCVerification req personId rcDetails
  return Success

handleDLVerification :: DriverOnBoardingReq -> Id SP.Person -> Maybe DDL.DriverDrivingLicense -> Flow ()
handleDLVerification req personId dl= do
  when (isNothing req.driverLicenseNumber || isNothing req.driverDateOfBirth) $ return ()
  now <- getCurrentTime
  case dl of
    Nothing -> do
      dlId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      dlEntity <- buildDBDL dlId personId req.driverDateOfBirth req.driverLicenseNumber idfyReqId now
      runTransaction $ QDDL.create dlEntity
      return ()
    Just dlRecord -> do
      dlNumber <- mapM decrypt dlRecord.driverLicenseNumber
      when (dlNumber /= req.driverLicenseNumber || dlRecord.driverDob /= req.driverDateOfBirth) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QDDL.resetDLRequest personId req.driverLicenseNumber req.driverDateOfBirth idfyReqId now


handleRCVerification ::  DriverOnBoardingReq -> Id SP.Person -> Maybe DVR.VehicleRegistrationCert -> Flow ()
handleRCVerification req personId rc = do
  when (isNothing req.vehicleRegistrationCertNumber) $ return ()
  now <- getCurrentTime
  case rc of
    Nothing -> do 
      rcId <- L.generateGUID
      let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
      rcEntity <- buildDBRC rcId personId req.vehicleRegistrationCertNumber idfyReqId now
      runTransaction $ QVR.create rcEntity
      return ()
    Just rcRecord -> do 
      rcNumber <- mapM decrypt rcRecord.vehicleRegistrationCertNumber
      when (rcNumber /= req.vehicleRegistrationCertNumber) do
        let idfyReqId = "idfy_req_id" :: Text -- replace by idfy api call
        runTransaction $ QVR.resetRCRequest personId req.driverLicenseNumber idfyReqId now


mkDBOP :: Text -> Text -> DriverOnBoardingReq -> UTCTime -> DOC.OperatingCity
mkDBOP opId orgId req time =
  DOC.OperatingCity
  {
    id = Id opId,
    organizationId = Id orgId,
    cityName = req.operatingCity,
    enabled = VALID,
    createdAt = time,
    updatedAt = time
  }

buildDBRC :: EncFlow m r => Text -> Id SP.Person -> Maybe Text -> Text -> UTCTime -> m DVR.VehicleRegistrationCert
buildDBRC opId personId rcNumber reqID time = do
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
      updatedAt = time
    }


buildDBDL :: EncFlow m r => Text -> Id SP.Person -> Maybe UTCTime -> Maybe Text -> Text -> UTCTime ->m DDL.DriverDrivingLicense
buildDBDL opId personId dob dlNumber reqId time = do
  ddl <- mapM encrypt dlNumber
  return $
    DDL.DriverDrivingLicense
      {
        id = Id opId,
        driverId = personId,
        driverDob = dob,
        driverLicenseNumber = ddl,
        driverLicenseStart = Nothing,
        idfyStatus = DVR.IN_PROGRESS,
        verificationStatus = DVR.PENDING,
        driverLicenseExpiry = Nothing,
        classOfVehicle = Nothing,
        request_id = reqId,
        createdAt = time,
        updatedAt = time
      }


-- createDDL :: (EncFlow m r, EsqDBFlow m r) => Id SP.Person -> DriverOnBoardingReq ->m DDL.DriverDrivingLicense
-- createDDL personId req = do
--   opId <- L.generateGUID
--   utcNow <- getCurrentTime
--   ddl <- mkDBOP2 opId personId req utcNow
--   runTransaction $ QDDL.create ddl
--   return ddl