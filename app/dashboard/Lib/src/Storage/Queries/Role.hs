module Storage.Queries.Role where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Role as Role
import Storage.Tabular.Role ()

create :: Role -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Role ->
  m (Maybe Role)
findById = Esq.findById
