module Storage.Queries.FareParameters where

import Domain.Types.FareParameters
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.FareParameters ()

create :: FareParameters -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id FareParameters -> m (Maybe FareParameters)
findById = Esq.findById
