module Storage.Queries.DriverReferral where

import Domain.Types.DriverReferral
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Prelude
import Storage.Tabular.DriverReferral ()

create :: DriverReferral -> SqlDB ()
create = Esq.create

findByRefferalCode :: Transactionable m => Id DriverReferral -> m (Maybe DriverReferral)
findByRefferalCode = Esq.findById


