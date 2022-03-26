module Storage.Queries.DunzoCreds where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.DunzoCreds
import Storage.Tabular.DunzoCreds ()

findById ::
  EsqDBFlow m r =>
  Id DunzoCreds ->
  m (Maybe DunzoCreds)
findById = Esq.findById
