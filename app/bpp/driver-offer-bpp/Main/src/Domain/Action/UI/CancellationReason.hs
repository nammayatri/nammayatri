module Domain.Action.UI.CancellationReason
  ( list,
  )
where

import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import qualified Storage.Queries.CancellationReason as QCR

list :: EsqDBReplicaFlow m r => m [SCR.CancellationReasonAPIEntity]
list = fmap SCR.makeCancellationReasonAPIEntity <$> runInReplica QCR.findAll
