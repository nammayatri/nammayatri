module Storage.Queries.OnSearchEvent where

import Domain.Types.OnSearchEvent
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.OnSearchEvent ()

create :: OnSearchEvent -> SqlDB ()
create = Esq.create
