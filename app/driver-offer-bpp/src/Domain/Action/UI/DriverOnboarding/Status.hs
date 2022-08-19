module Domain.Action.UI.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes (..),
    statusImpl,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (EsqDBFlow)
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Error
import Beckn.Types.Id (Id)
import Beckn.Utils.Error
import qualified Domain.Types.DriverOnboarding.ClassOfVehicle as COV
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.DriverOnboarding.DriverLicense as QDDL
import qualified Storage.Queries.DriverOnboarding.OperatingCity as DO
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as DVehicle
import Storage.Queries.Person as Person
import qualified Storage.Queries.Person as QPerson

data ResponseStatus = VERIFICATION_PENDING | VERIFIED | VERIFICATION_FAILED | WAITING_INPUT
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

data StatusRes = StatusRes
  { dlVerificationStatus :: ResponseStatus,
    rcVerificationStatus :: ResponseStatus,
    operatingCity :: Text
  }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

statusImpl :: (EsqDBFlow m r) => Id SP.Person -> m StatusRes
statusImpl personId = do
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
