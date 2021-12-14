module Storage.Queries.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Search
import Storage.Tabular.Search ()

findById :: EsqDBFlow m r => Id Search -> m (Maybe Search)
findById = Esq.findById

create :: Search -> SqlDB ()
create = create'
