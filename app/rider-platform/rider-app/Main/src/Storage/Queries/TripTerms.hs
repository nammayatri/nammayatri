module Storage.Queries.TripTerms where

import Domain.Types.TripTerms
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Tabular.TripTerms

findById' :: (MonadThrow m, Log m, Transactionable m) => Id TripTerms -> DTypeBuilder m (Maybe TripTermsT)
findById' = Esq.findById'
