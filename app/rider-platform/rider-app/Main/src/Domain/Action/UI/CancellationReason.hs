module Domain.Action.UI.CancellationReason
  ( list,
  )
where

import qualified Domain.Types.CancellationReason as DCR
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto (runInReplica)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Storage.Queries.CancellationReason as QCR

list :: EsqDBReplicaFlow m r => DCR.CancellationStage -> m [DCR.CancellationReasonAPIEntity]
list cancStage = do
  map DCR.makeCancellationReasonAPIEntity <$> runInReplica (QCR.findAll cancStage)
