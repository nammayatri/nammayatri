module Storage.Queries.Driveronboarding.VehicleRegistrationCert where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Storage.Tabular.Driveronboarding.VehicleRegistrationCert ()
create :: VehicleRegistrationCert -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id VehicleRegistrationCert ->
  m (Maybe VehicleRegistrationCert)
findById = Esq.findById


