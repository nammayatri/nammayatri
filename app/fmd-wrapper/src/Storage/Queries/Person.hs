module Storage.Queries.Person where

import Beckn.Storage.Esqueleto as Esq
import Domain.Person
import Storage.Tabular.Person ()

create :: Person -> SqlDB ()
create = create'
