module Storage.Queries.RentalSlab where

import Domain.Types.RentalSlab
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.RentalSlab

findById' :: (MonadThrow m, Log m, Transactionable m) => Id RentalSlab -> DTypeBuilder m (Maybe RentalSlabT)
findById' = Esq.findById'
