module Storage.Queries.CancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Domain.Types.CancellationReason
import Storage.Tabular.CancellationReason

findAll :: EsqDBFlow m r => CancellationStage -> m [CancellationReason]
findAll cancStage =
  runTransaction . findAll' $ do
    cancellationReason <- from $ table @CancellationReasonT
    where_ $
      cancellationReason ^. CancellationReasonEnabled
        &&. case cancStage of
          OnSearch -> cancellationReason ^. CancellationReasonOnSearch
          OnConfirm -> cancellationReason ^. CancellationReasonOnConfirm
          OnAssign -> cancellationReason ^. CancellationReasonOnAssign
    return cancellationReason
