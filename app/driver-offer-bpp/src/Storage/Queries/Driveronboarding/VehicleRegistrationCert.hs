module Storage.Queries.Driveronboarding.VehicleRegistrationCert where

import Beckn.Prelude
import Beckn.External.Encryption

import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Storage.Tabular.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Driveronboarding.VehicleRegistrationCert
import Domain.Types.Person (Person)
import Beckn.Types.Common (Log)
import Storage.Tabular.Person ()

create :: VehicleRegistrationCert -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id VehicleRegistrationCert ->
  m (Maybe VehicleRegistrationCert)
findById = Esq.findById

 
findByPId ::
  (MonadThrow m, Log m, Transactionable m, EncFlow m r) =>
  Id Person ->
  m (Maybe VehicleRegistrationCert)
findByPId personid = do
  findOne $ do
    vechileRegCert <- from $ table @VehicleRegistrationCertT
    where_ $ vechileRegCert ^. VehicleRegistrationCertDriverId ==. val (toKey personid)
    return vechileRegCert
    
