module Storage.Queries.BookingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.BookingLocation
import Storage.Tabular.BookingLocation ()

create :: BookingLocation -> SqlDB ()
create = create'

findById ::
  Transactionable m =>
  Id BookingLocation ->
  m (Maybe BookingLocation)
findById = Esq.findById
