module Storage.Queries.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Search
import Storage.Tabular.Search

findById :: Transactionable m => Id Search -> m (Maybe Search)
findById searchId =
  Esq.findOne $ do
    search <- from $ table @SearchT
    where_ $ search ^. SearchId ==. val (getId searchId)
    return search

create :: Search -> SqlDB ()
create = Esq.create
