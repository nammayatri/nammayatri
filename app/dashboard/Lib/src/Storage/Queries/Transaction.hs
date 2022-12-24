module Storage.Queries.Transaction where

import Beckn.Storage.Esqueleto as Esq
import Domain.Types.Transaction as T
import Storage.Tabular.Transaction ()

create :: Transaction -> SqlDB ()
create = Esq.create
