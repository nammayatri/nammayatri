module Storage.Queries.OnSearchEvent where

import Beckn.Storage.Esqueleto as Esq
import Domain.Types.OnSearchEvent
import Storage.Tabular.OnSearchEvent ()

create :: OnSearchEvent -> SqlDB ()
create = Esq.create
