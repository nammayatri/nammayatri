module Storage.Queries.SearchRetry where

import qualified Domain.Types.SearchRetry as SearchRetry
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.SearchRetry ()

create :: SearchRetry.SearchRetry -> SqlDB ()
create = Esq.create
