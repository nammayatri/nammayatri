{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Product.DriveronBoarding.Status where
import Beckn.Types.Id (Id)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as DVehicle
import Beckn.Prelude
import Beckn.Utils.Error
import Beckn.Types.Error
import qualified Domain.Types.Person as SP
import Storage.Queries.Person as Person
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import qualified Storage.Queries.Driveronboarding.OperatingCity as DO
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as VRC
import Environment ( FlowHandler )    
import Types.API.Driveronboarding.Status
import Domain.Types.Driveronboarding.OperatingCity
import qualified Beckn.Storage.Esqueleto as DB


statusHandler :: Id SP.Person -> FlowHandler StatusRes
statusHandler personId = withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    vehicleRegCert <- DVehicle.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
    driverDrivingLicense <- QDDL.findByDId personId >>= fromMaybeM (PersonNotFound personId.getId)
    operatinCity <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
    let vehicleRCVerification = getVerificationStatus vehicleRegCert.verificationStatus
    let driverDLVerification = getVerificationStatus driverDrivingLicense.verificationStatus
    let operatingCityVerification = operatinCity.enabled
    let response = StatusRes vehicleRCVerification driverDLVerification operatingCityVerification
    case (vehicleRCVerification,driverDLVerification,operatingCityVerification) of
        (VERIFIED,VERIFIED,VALID) -> DB.runTransaction $ Person.setRegisteredTrue personId   
    return response

getVerificationStatus :: VRC.VerificationStatus -> ResponseStatus
getVerificationStatus = \case 
    VRC.PENDING -> PENDINGVERIFICATION
    VRC.VALID -> VERIFIED
    VRC.INVALID -> FAILEDVERIFICATION 
    VRC.NOTFOUND -> WAITING_INPUT