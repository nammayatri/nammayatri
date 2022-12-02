module Storage.Queries.RentalSlab where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.RentalSlab
import Storage.Tabular.RentalSlab

findById' :: (MonadThrow m, Log m, Transactionable m) => Id RentalSlab -> DTypeBuilder m (Maybe RentalSlabT)
findById' = Esq.findById'
