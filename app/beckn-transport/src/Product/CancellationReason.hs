module Product.CancellationReason
  ( list,
  )
where

import App.Types
import Beckn.Types.Id
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.Storage.CancellationReason as SCR
import qualified Types.Storage.Person as Person
import Utils.Common

list :: Id Person.Person -> FlowHandler CancellationReasonAPI.ListRes
list _ = withFlowHandlerAPI $ do
  (toCancellationReasonEntity <$>) <$> QCR.findAll
  where
    toCancellationReasonEntity SCR.CancellationReason {..} =
      CancellationReasonAPI.CancellationReasonEntity {..}
