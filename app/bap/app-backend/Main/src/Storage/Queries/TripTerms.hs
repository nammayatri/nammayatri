module Storage.Queries.TripTerms where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.TripTerms
import Storage.Tabular.TripTerms

findById' :: (MonadThrow m, Log m, Transactionable m) => Id TripTerms -> DTypeBuilder m (Maybe TripTermsT)
findById' = Esq.findById'
