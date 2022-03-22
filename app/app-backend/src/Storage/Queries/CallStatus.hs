module Storage.Queries.CallStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.CallStatus
import Storage.Tabular.CallStatus ()

create :: CallStatus -> SqlDB ()
create = create'

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById
