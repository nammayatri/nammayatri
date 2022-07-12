module Storage.Queries.Driveronboarding.DriverDrivingLicense where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Driveronboarding.DriverDrivingLicense
import Domain.Types.Person (Person)
import Storage.Tabular.Person ()
import Storage.Tabular.Driveronboarding.DriverDrivingLicense
import Beckn.Types.Id
import Domain.Types.Driveronboarding.VehicleRegistrationCert (COV, IdfyStatus)

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

updateDLDetails :: Text -> Maybe UTCTime -> Maybe UTCTime -> IdfyStatus -> Maybe [COV] -> SqlDB () -- [COV] changed to Maybe [COV]
updateDLDetails requestId start expiry status cov = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverDrivingLicenseClassOfVehicle =. val cov,
        DriverDrivingLicenseDriverLicenseStart =. val start,
        DriverDrivingLicenseDriverLicenseExpiry =. val expiry,
        DriverDrivingLicenseDriverLicenseStatus =. val status
      ]
    where_ $ tbl ^. DriverDrivingLicenseRequest_id  ==. val requestId
