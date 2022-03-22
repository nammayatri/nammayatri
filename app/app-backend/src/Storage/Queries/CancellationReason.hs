module Storage.Queries.CancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Types.CancellationReason
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
    return cancellationReason
