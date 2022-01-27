module Storage.Queries.OnSearchEvent where

import Beckn.Storage.Esqueleto
import Domain.Types.OnSearchEvent
import Storage.Tabular.OnSearchEvent ()

create :: OnSearchEvent -> SqlDB ()
create = do
  create'
