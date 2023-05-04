module Storage.Queries.SearchRequestRetry where

import qualified Domain.Types.SearchRequestRetry as SearchRequestRetry
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.SearchRequestRetry ()

create :: SearchRequestRetry.SearchRequestRetry -> SqlDB ()
create = Esq.create
