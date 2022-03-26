module Storage.Queries.Address where

import Beckn.Storage.Esqueleto as Esq
import Domain.Address
import Storage.Tabular.Address ()

create :: Address -> SqlDB ()
create = create'
