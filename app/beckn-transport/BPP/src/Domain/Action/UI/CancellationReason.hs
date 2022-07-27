module Domain.Action.UI.CancellationReason
  ( ListRes,
    list,
  )
where

import qualified Domain.Types.CancellationReason as SCR
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import Utils.Common

type ListRes = [SCR.CancellationReasonAPIEntity]

list :: (EsqDBFlow m r) => m ListRes
list = do
  fmap SCR.makeCancellationReasonAPIEntity <$> QCR.findAll
