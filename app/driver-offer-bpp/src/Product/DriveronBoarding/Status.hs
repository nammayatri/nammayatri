module Product.DriveronBoarding.Status where
import Beckn.Types.Id (Id)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as DVehicle
import Beckn.Prelude
import Beckn.Utils.Error
import Beckn.Types.Error
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import qualified Storage.Queries.Driveronboarding.OperatingCity as DO
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as VRC
import Environment ( FlowHandler )    
import Types.API.Driveronboarding.Status
import Beckn.Types.APISuccess
import Domain.Types.Driveronboarding.OperatingCity



-- handle not found scenerio

function :: Id SP.Person -> FlowHandler StatusRes
function personId = withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    vehicleRegCert <- DVehicle.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
    driverDrivingLicense <- QDDL.findByDId personId >>= fromMaybeM (PersonNotFound personId.getId)
    operatinCity <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
    let rcVerification = getVerificationStatus vehicleRegCert.verificationStatus
    let ddlVerification = getVerificationStatus driverDrivingLicense.verificationStatus
    let opVerification = operatinCity.enabled
    let hello = StatusRes rcVerification ddlVerification opVerification
    return hello

example :: ResponseStatus -> ResponseStatus ->  OperatingCityVerification -> FlowHandler APISuccess   
example = do
    let trying = example
    pure Success

getVerificationStatus :: VRC.VerificationStatus -> ResponseStatus
getVerificationStatus = \case 
    VRC.PENDING -> PENDINGVERIFICATION
    VRC.VALID -> FAILEDVERIFICATION
    VRC.INVALID -> FAILEDVERIFICATION 