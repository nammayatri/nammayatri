module Storage.Queries.RideDetails where

import qualified Domain.Types.Ride as SR
import Domain.Types.RideDetails
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.RideDetails ()

create :: RideDetails -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id SR.Ride ->
  m (Maybe RideDetails)
findById = Esq.findById
