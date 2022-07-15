module Product.DriveronBoarding.Status where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import qualified Domain.Types.Driveronboarding.VehicleRegistrationCert as VRC
import qualified Domain.Types.Person as SP
import Environment (FlowHandler)
import qualified Storage.Queries.Driveronboarding.DriverDrivingLicense as QDDL
import qualified Storage.Queries.Driveronboarding.OperatingCity as DO
import qualified Storage.Queries.Driveronboarding.VehicleRegistrationCert as DVehicle
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson
import Types.API.Driveronboarding.Status

statusHandler :: Id SP.Person -> FlowHandler StatusRes
statusHandler personId = withFlowHandlerAPI $ do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  vehicleRegCertM <- DVehicle.findByPId personId
  driverDrivingLicenseM <- QDDL.findByDId personId
  operatingCity <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
  let vehicleRCVerification = getVerificationStatus ((.verificationStatus) <$> vehicleRegCertM)
  let driverDLVerification = getVerificationStatus ((.verificationStatus) <$> driverDrivingLicenseM)
  let operatingCityVerification = operatingCity.cityName
  let response = StatusRes vehicleRCVerification driverDLVerification operatingCityVerification
  when (vehicleRCVerification == VERIFIED || driverDLVerification == VERIFIED) $ DB.runTransaction $ Person.setRegisteredTrue personId
  return response

getVerificationStatus :: Maybe VRC.VerificationStatus -> ResponseStatus
getVerificationStatus = \case
  Just VRC.PENDING -> VERIFICATION_PENDING
  Just VRC.VALID -> VERIFIED
  Just VRC.INVALID -> VERIFICATION_FAILED
  Nothing -> WAITING_INPUT
