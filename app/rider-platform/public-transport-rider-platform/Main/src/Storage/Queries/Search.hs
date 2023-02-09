module Storage.Queries.Search where

import Domain.Types.Search
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Search

findById :: Transactionable m => Id Search -> m (Maybe Search)
findById searchId =
  Esq.findOne $ do
    search <- from $ table @SearchT
    where_ $ search ^. SearchId ==. val (getId searchId)
    return search

create :: Search -> SqlDB ()
create = Esq.create
