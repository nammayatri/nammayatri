module Storage.Queries.DriverQuote where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import qualified Domain.Types.DriverQuote as Domain
import Storage.Tabular.DriverQuote ()

create :: Domain.DriverQuote -> SqlDB ()
create = Esq.create

findById :: (Transactionable m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById = Esq.findById
