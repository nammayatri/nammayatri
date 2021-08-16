module Product.CancellationReason
  ( list,
  )
where

import App.Types
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.Storage.CancellationReason as SCR
import Utils.Common

list :: FlowHandler CancellationReasonAPI.ListRes
list = withFlowHandlerAPI $ do
  (toCancellationReasonEntity <$>) <$> QCR.findAll
  where
    toCancellationReasonEntity SCR.CancellationReason {..} =
      CancellationReasonAPI.CancellationReasonEntity {..}
