{-# LANGUAGE TypeApplications #-}
module Storage.Queries.Driveronboarding.DriverDrivingLicense where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Driveronboarding.DriverDrivingLicense
import Domain.Types.Driveronboarding.VehicleRegistrationCert (COV (..), IdfyStatus (..), VerificationStatus (..))
import Domain.Types.Person (Person)
import Storage.Tabular.Driveronboarding.DriverDrivingLicense
import Storage.Tabular.Person ()

create :: DriverDrivingLicense -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverDrivingLicense ->
  m (Maybe DriverDrivingLicense)
findById = Esq.findById

findByDId ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverDrivingLicense)
findByDId personid = do
  findOne $ do
    driverDriving <- from $ table @DriverDrivingLicenseT
    where_ $ driverDriving ^. DriverDrivingLicenseDriverId ==. val (toKey personid)
    return driverDriving

updateDLDetails :: Text -> Maybe UTCTime -> Maybe UTCTime -> IdfyStatus -> VerificationStatus -> Maybe [COV] -> UTCTime -> SqlDB () -- [COV] changed to Maybe [COV]
updateDLDetails idfyRequestId start expiry idfyStatus verificationStatus cov now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverDrivingLicenseClassOfVehicle =. val cov,
        DriverDrivingLicenseDriverLicenseStart =. val start,
        DriverDrivingLicenseDriverLicenseExpiry =. val expiry,
        DriverDrivingLicenseIdfyStatus =. val idfyStatus,
        DriverDrivingLicenseVerificationStatus =. val verificationStatus,
        DriverDrivingLicenseUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverDrivingLicenseIdfyRequestId ==. val idfyRequestId

resetDLRequest :: Id Person -> Maybe Text -> Maybe UTCTime -> Text -> UTCTime -> SqlDB ()
resetDLRequest driverId dlNumber dob idfyRequestId now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverDrivingLicenseDriverLicenseNumber =. val dlNumber,
        DriverDrivingLicenseDriverDob =. val dob,
        DriverDrivingLicenseIdfyRequestId =. val idfyRequestId,
        DriverDrivingLicenseClassOfVehicle =. val Nothing,
        DriverDrivingLicenseDriverLicenseStart =. val Nothing,
        DriverDrivingLicenseDriverLicenseExpiry =. val Nothing,
        DriverDrivingLicenseIdfyStatus =. val IN_PROGRESS,
        DriverDrivingLicenseVerificationStatus =. val PENDING,
        DriverDrivingLicenseUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverDrivingLicenseDriverId ==. val (toKey driverId)
