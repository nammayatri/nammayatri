module Product.DriveronBoarding.Status where
import Beckn.Types.Id (Id)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as DVehicle
import Beckn.Prelude
import Beckn.Utils.Error
import Beckn.Types.Error
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as DDLI
import qualified Storage.Queries.Driveronboarding.OperatingCity as DO
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as VRC
import Environment ( FlowHandler )    
import Types.API.Driveronboarding.Status

ndData :: Id SP.Person -> FlowHandler StatusRes
ndData personId = withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    abc <- DVehicle.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
    def <- DDLI.findByDId personId >>= fromMaybeM (PersonNotFound personId.getId)
    ghi <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
    let rcveri = getVerificationStatus abc.rcStatus
    let dlveri = getVerificationStatus def.driverLicenseStatus
    let opcveri = ghi.enabled
    let hello = StatusRes dlveri rcveri opcveri
    return hello

getVerificationStatus :: VRC.IdfyStatus -> VerificationStatus
getVerificationStatus = \case 
    VRC.IN_PROGRESS -> PENDINGVERIFICATION
    VRC.FAILED -> FAILEDVERIFICATION
    VRC.COMPLETED -> FAILEDVERIFICATION
    VRC.VALID -> VERIFIED
    VRC.INVALID -> FAILEDVERIFICATION