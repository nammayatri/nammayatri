module Product.CancellationReason
  ( list,
  )
where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import qualified Types.API.CancellationReason as CancellationReasonAPI
import Utils.Common

list :: Id Person.Person -> DCR.CancellationStage -> FlowHandler CancellationReasonAPI.ListRes
list _ cancStage = withFlowHandlerAPI $ do
  map DCR.makeCancellationReasonAPIEntity <$> QCR.findAll cancStage
