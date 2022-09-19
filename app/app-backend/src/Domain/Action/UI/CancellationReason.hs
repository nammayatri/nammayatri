module Domain.Action.UI.CancellationReason
  ( list,
  )
where

import qualified Domain.Types.CancellationReason as DCR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import Utils.Common

list :: EsqDBFlow m r => DCR.CancellationStage -> m [DCR.CancellationReasonAPIEntity]
list cancStage = do
  map DCR.makeCancellationReasonAPIEntity <$> QCR.findAll cancStage
