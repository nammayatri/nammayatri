module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.CancellationReason

findAll :: Transactionable m => CancellationStage -> m [CancellationReason]
findAll cancStage =
  Esq.findAll $ do
    cancellationReason <- from $ table @CancellationReasonT
    where_ $
      cancellationReason ^. CancellationReasonEnabled
        &&. case cancStage of
          OnSearch -> cancellationReason ^. CancellationReasonOnSearch
          OnConfirm -> cancellationReason ^. CancellationReasonOnConfirm
          OnAssign -> cancellationReason ^. CancellationReasonOnAssign
    orderBy [desc $ cancellationReason ^. CancellationReasonPriority]
    return cancellationReason
