module Storage.Queries.FareParameters where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FareParameters
import Storage.Tabular.FareParameters ()

create :: FareParameters' -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id FareParameters -> m (Maybe FareParameters')
findById = Esq.findById
