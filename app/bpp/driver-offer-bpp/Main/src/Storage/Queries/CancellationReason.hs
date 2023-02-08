module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason hiding (priority)
import Kernel.Prelude hiding (isNothing)
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.CancellationReason

findAll :: Transactionable m => m [CancellationReason]
findAll = Esq.findAll $ do
  cancellationReason <- from $ table @CancellationReasonT
  where_ $ cancellationReason ^. CancellationReasonEnabled
  orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
  return cancellationReason
