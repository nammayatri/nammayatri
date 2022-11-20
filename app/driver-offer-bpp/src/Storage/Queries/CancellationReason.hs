module Storage.Queries.CancellationReason where

import Beckn.Prelude hiding (isNothing)
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.CancellationReason hiding (priority)
import Storage.Tabular.CancellationReason

findAll :: Transactionable m => m [CancellationReason]
findAll = Esq.findAll $ do
  cancellationReason <- from $ table @CancellationReasonT
  where_ $ cancellationReason ^. CancellationReasonEnabled
  orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
  return cancellationReason