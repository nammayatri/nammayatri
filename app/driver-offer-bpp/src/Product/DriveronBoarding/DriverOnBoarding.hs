{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

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
import Beckn.Types.Error (PersonError(PersonFieldNotPresent, PersonNotFound), OrganizationError (OrgNotFound))
import qualified Storage.Queries.Organization as QOrganization
import qualified Storage.Queries.Person as QPerson
import Types.API.Driveronboarding.DriverOnBoarding
import Domain.Types.Driveronboarding.VehicleRegistrationCert as DOV hiding (VALID)
import Storage.Queries.Driveronboarding.VehicleRegistrationCert  as DVR
import Domain.Types.Driveronboarding.DriverDrivingLicense as DDL
import Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import Beckn.Types.APISuccess

registrationHandler1 :: Id SP.Person -> DriverOnBoardingReq -> FlowHandler DriverOnBoardingRes
registrationHandler1 personId request@DriverOnBoardingReq {..}= withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    organization <-QOrganization.findById orgId
      >>= fromMaybeM (OrgNotFound orgId.getId)
    let orgTxt = getId organization.id    
    opId <- L.generateGUID
    task_id <- L.generateGUID -- task_id for idfy request
    let group_id = personId   -- group_id for idfy request
    utcNow <- getCurrentTime
    runTransaction $ QOC.create (mkDBOP opId orgTxt request utcNow)
    runTransaction $ DVR.create (mkDBOP1 opId personId request utcNow)    
    runTransaction $ QDDL.create (mkDBOP2 opId personId request utcNow)    

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

mkDBOP1 :: Text -> Id SP.Person -> DriverOnBoardingReq -> UTCTime -> DOV.VehicleRegistrationCert
mkDBOP1 opId personId req time = 
    DOV.VehicleRegistrationCert
    {
      id = Id opId,
      driverId = personId,
      vehicleRegistrationCertNumber = req.vehicleRegistrationCertNumber,
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

mkDBOP2 :: Text -> Id SP.Person -> DriverOnBoardingReq -> UTCTime -> DDL.DriverDrivingLicense
mkDBOP2 opId personId req time = 
  DDL.DriverDrivingLicense
    {
      id = Id opId,
      driverId = personId,
      driverLicenseNumber = req.driverLicenseNumber,
      driverLicenseStart = Just time,
      driverLicenseStatus = FAILED,
      driverVerificationStatus = Just INVALID,
      driverLicenseExpiry = Just time,
      classOfVehicle = Nothing,
      request_id = "WAITING",
      createdAt = time,
      updatedAt = time
    }

