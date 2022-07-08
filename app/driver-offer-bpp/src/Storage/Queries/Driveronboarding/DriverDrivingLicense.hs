module Storage.Queries.Driveronboarding.DriverDrivingLicense where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Driveronboarding.DriverDrivingLicense
import Storage.Tabular.Driveronboarding.DriverDrivingLicense ()
import Beckn.Types.Id

create :: DriverDrivingLicense -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverDrivingLicense ->
  m (Maybe DriverDrivingLicense)
findById = Esq.findById











