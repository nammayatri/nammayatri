module Storage.Queries.RideDetails where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.Ride as SR
import Domain.Types.RideDetails
import Storage.Tabular.RideDetails ()

create :: RideDetails -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id SR.Ride ->
  m (Maybe RideDetails)
findById = Esq.findById
