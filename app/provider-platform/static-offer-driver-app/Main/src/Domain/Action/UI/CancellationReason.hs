module Domain.Action.UI.CancellationReason
  ( ListRes,
    list,
  )
where

import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable
import qualified Storage.Queries.CancellationReason as QCR

type ListRes = [SCR.CancellationReasonAPIEntity]

list :: (EsqDBReplicaFlow m r) => m ListRes
list = do
  fmap SCR.makeCancellationReasonAPIEntity <$> runInReplica QCR.findAll
