module Storage.Queries.SearchRetry where

import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchRetry
import qualified Domain.Types.SearchRetry as SearchRetry
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.SearchRetry ()

create :: SearchRetry.SearchRetry -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id SearchRequest -> m (Maybe SearchRetry)
findById = Esq.findById
