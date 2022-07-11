
module Types.API.Driveronboarding.Status where
import Beckn.Types.Id (Id)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as DVehicle
import Beckn.Prelude
import Beckn.Utils.Error
import Beckn.Types.Error
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as DDLI
import Domain.Types.Driveronboarding.DriverDrivingLicense as DD
import qualified Storage.Queries.Driveronboarding.OperatingCity as DO
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Driveronboarding.OperatingCity

import Environment ( FlowHandler )



data StatusRes = StatusRes
    {
        dlVerificationStatus :: Verification1,
        rcVerificationStatus :: Verification2,
        operatingCityStatus :: OperatingCityVerification
   
    }
    deriving ( Show,Eq,Read,Generic,ToJSON,FromJSON,ToSchema)

-- data DriverDrivingLicense = DriverDrivingLicense {
--     id :: Id DriverDrivingLicense,
--     driverId :: Id Person,
--     driverLicenseNumber :: Maybe Text,
--     driverLicenseStart :: Maybe UTCTime,
--     driverLicenseStatus :: DLStatus,
--     driverVerificationStatus :: Maybe DriverLicenseStatus,
--     driverLicenseExpiry :: Maybe UTCTime,
--     classOfVehicle :: [VehicleClass],
--     request_id :: Text,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
-- }
--     deriving (Generic)

-- getStatus :: Id SP.Person -> m StatusRes
-- getStatus personId = do
--     person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
--     orgId <- person.organizationId & fromMaybeM (OrgFieldNotPresent "organization_id")
--     abc <- DVehicle.findByPId personId fromMaybeM (PersonNotFound orgId.getId)
--     def <- DDL.findById personId fromMaybeM (OrgFieldNotPresent orgId.getId)
--     ghi <- DOC.findById orgId fromMaybeM (OrgFieldNotPresent orgId.getId)
--     rcveri <- abc.vehicleRegStatus
--     dlveri <- abc.driverLicenseStatus
--     opcveri <- abc.enabled
--     return (StatusRes rcveri dlveri enabled)
    
      

ndData :: Id SP.Person -> FlowHandler StatusRes
ndData personId = withFlowHandlerAPI $ do
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
    abc <- DVehicle.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
    def <- DDLI.findByDId personId >>= fromMaybeM (PersonNotFound personId.getId)
    ghi <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
    let rcveri = abc.vehicleRegStatus
    let dlveri = def.driverLicenseStatus
    let opcveri = ghi.enabled
    let hello = (StatusRes dlveri rcveri opcveri)
    return hello























