{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Product.DriveronBoarding.DriverOnBoarding where
import Environment

import Domain.Types.Driveronboarding.OperatingCity as DO
import Beckn.Types.Id
import qualified Storage.Queries.Driveronboarding.OperatingCity as QOC
import Domain.Types.Driveronboarding.OperatingCity as DOP
import qualified EulerHS.Language as L
import Beckn.Storage.Esqueleto
import Utils.Common
import Prelude
import Beckn.Prelude
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import Types.API.Driveronboarding.DriverOnBoarding
import Domain.Types.Driveronboarding.VehicleRegistrationCert as DOV hiding (VALID)
import Storage.Queries.Driveronboarding.VehicleRegistrationCert  as DVR
import Domain.Types.Driveronboarding.DriverDrivingLicense as DDL
import Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
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
registrationHandler1 personId request@DriverOnBoardingReq {..}= withFlowHandlerAPI $ do
  runRequestValidation validateIssue1 DriverOnBoardingReq {..}  
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (OrgFieldNotPresent "organization_id")
  organization <-QOrganization.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  let orgTxt = getId organization.id    
  opId <- L.generateGUID
  task_id <- L.generateGUID -- task_id for idfy request
  let group_id = personId   -- group_id for idfy request
  utcNow <- getCurrentTime
  dlldetails <- QDDL.findByDId personId
  vrcdetails <- DVR.findByPId personId
  -- if (dlldetails == Nothing)
  --   then do
  --     ddlCall <- (mkDBOP2 opId personId request utcNow)     
  --     runTransaction $ QDDL.create ddlCall
  -- else do
  --   ddlCall <- (mkDBOP2 opId personId request utcNow)    
  --   if (ddlCall.driverLicenseNumber /= driverLicenseNumber)
  --     then      
  --       runTransaction $ QDDL.update driverLicenseNumber
  --   else if (ddlCall.driverDateOfBirth /= driverDateOfBirth)
  --     then 
  --       runTransaction $ QDDL.update driverDateOfBirth





  -- hello <- (mkDBOP2 opId personId request utcNow)   
  runTransaction $ QOC.create (mkDBOP opId orgTxt request utcNow)
  -- vrcCall <- (mkDBOP1 opId personId request utcNow)  
  -- runTransaction $ DVR.create vrcCall
  -- idify req
  -- reqid -> store table lo insert
    -- validatingDLL <- case (driverLicenseNumber, driverDateOfBirth) of
    --   (Nothing, Nothing) -> throwError $ InvalidRequest "You should pass registration number and vehicle id."
    -- update specific field
    
  return Success

mkDBOP :: Text -> Text -> DriverOnBoardingReq -> UTCTime -> DOP.OperatingCity
mkDBOP opId orgId req time = 
    DOP.OperatingCity
    {
      id = Id opId,
      organizationId = Id orgId,
      cityName = req.operatingCity,
      enabled = VALID,
      createdAt = time, 
      updatedAt = time  
    } 

mkDBOP1 :: (EncFlow m r, EsqDBFlow m r) => Text -> Id SP.Person -> DriverOnBoardingReq -> UTCTime ->m DOV.VehicleRegistrationCert
mkDBOP1 opId personId req time = do
  vrc <- mapM encrypt (req.vehicleRegistrationCertNumber)
  return $
    DOV.VehicleRegistrationCert
    {
      id = Id opId,
      driverId = personId,
      vehicleRegistrationCertNumber = vrc,
      fitnessCertExpiry = Just time,
      permitNumber = Nothing,
      permitStart = Just time,
      permitExpiry = Just time,
      vehicleClass = Nothing,
      request_id = "WAITING",
      vehicleNumber = Nothing,
      insuranceValidity = Just time,
      createdAt = time, 
      rcStatus = IN_PROGRESS,
      updatedAt = time  
    }      
     
      
mkDBOP2 :: (EncFlow m r, EsqDBFlow m r) => Text -> Id SP.Person -> DriverOnBoardingReq -> UTCTime ->m DDL.DriverDrivingLicense
mkDBOP2 opId personId req time = do
  ddl <- mapM encrypt (req.driverLicenseNumber)
  return $
    DDL.DriverDrivingLicense
      {
        id = Id opId,
        driverId = personId,
        driverLicenseNumber = ddl,
        driverLicenseStart = Just time,
        driverLicenseStatus = FAILED,
        driverVerificationStatus = Just INVALID,
        driverLicenseExpiry = Just time,
        classOfVehicle = Nothing,
        request_id = "WAITING",
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