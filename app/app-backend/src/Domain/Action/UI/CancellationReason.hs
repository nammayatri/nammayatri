module Domain.Action.UI.CancellationReason
  ( list,
  )
where

import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Domain.Types.CancellationReason as DCR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR

list :: EsqDBReplicaFlow m r => DCR.CancellationStage -> m [DCR.CancellationReasonAPIEntity]
list cancStage = do
  map DCR.makeCancellationReasonAPIEntity <$> QCR.findAll cancStage
