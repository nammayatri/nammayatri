module Product.DriverOnboarding.Status where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as COV
import qualified Domain.Types.Person as SP
import Environment (FlowHandler)
import qualified Storage.Queries.DriverOnboarding.DriverLicense as QDDL
import qualified Storage.Queries.DriverOnboarding.OperatingCity as DO
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as DVehicle
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson
import Types.API.DriverOnboarding.Status

statusHandler :: Id SP.Person -> FlowHandler StatusRes
statusHandler personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  vehicleRegCertM <- DVehicle.findByPersonId personId
  driverDrivingLicenseM <- QDDL.findByDriverId personId
  operatingCity <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
  let vehicleRCVerification = getVerificationStatus ((.verificationStatus) <$> vehicleRegCertM)
  let driverDLVerification = getVerificationStatus ((.verificationStatus) <$> driverDrivingLicenseM)
  let operatingCityVerification = operatingCity.cityName
  let response = StatusRes vehicleRCVerification driverDLVerification operatingCityVerification
  when (vehicleRCVerification == VERIFIED || driverDLVerification == VERIFIED) $ DB.runTransaction $ Person.setRegisteredTrue personId
  return response

getVerificationStatus :: Maybe COV.VerificationStatus -> ResponseStatus
getVerificationStatus = \case
  Just COV.PENDING -> VERIFICATION_PENDING
  Just COV.VALID -> VERIFIED
  Just COV.INVALID -> VERIFICATION_FAILED
  Nothing -> WAITING_INPUT
