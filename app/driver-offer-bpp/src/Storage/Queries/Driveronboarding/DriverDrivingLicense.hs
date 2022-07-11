module Storage.Queries.Driveronboarding.DriverDrivingLicense where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.External.Encryption
import Domain.Types.Driveronboarding.DriverDrivingLicense
import Storage.Tabular.Driveronboarding.DriverDrivingLicense 
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Beckn.Types.Common (Log)
import Storage.Tabular.Person ()

create :: DriverDrivingLicense -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverDrivingLicense ->
  m (Maybe DriverDrivingLicense)
findById = Esq.findById

findByDId ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Id Person ->
  m (Maybe DriverDrivingLicense)
findByDId personid = do
  findOne $ do
    driverDriving <- from $ table @DriverDrivingLicenseT 
    where_ $ driverDriving ^. DriverDrivingLicenseDriverId ==. val (toKey personid)
    return driverDriving









