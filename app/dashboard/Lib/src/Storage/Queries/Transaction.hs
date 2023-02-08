module Storage.Queries.Transaction where

import Domain.Types.Transaction as T
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.Transaction ()

create :: Transaction -> SqlDB ()
create = Esq.create
