module Product.DriveronBoarding.Status where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import Domain.Types.Driveronboarding.OperatingCity
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
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organization_id")
  vehicleRegCert <- DVehicle.findByPId personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverDrivingLicense <- QDDL.findByDId personId >>= fromMaybeM (PersonNotFound personId.getId)
  operatinCity <- DO.findByorgId orgId >>= fromMaybeM (PersonNotFound orgId.getId)
  let vehicleRCVerification = getVerificationStatus vehicleRegCert.verificationStatus
  let driverDLVerification = getVerificationStatus driverDrivingLicense.verificationStatus
  let operatingCityVerification = operatinCity.enabled
  let response = StatusRes vehicleRCVerification driverDLVerification operatingCityVerification
  when (vehicleRCVerification == VERIFIED || driverDLVerification == VERIFIED || operatingCityVerification == VALID) $ DB.runTransaction $ Person.setRegisteredTrue personId
  return response

getVerificationStatus :: VRC.VerificationStatus -> ResponseStatus
getVerificationStatus = \case
  VRC.PENDING -> PENDINGVERIFICATION
  VRC.VALID -> VERIFIED
  VRC.INVALID -> FAILEDVERIFICATION
  VRC.NOTFOUND -> WAITING_INPUT
