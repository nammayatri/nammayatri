module Product.CancellationReason
  ( list,
  )
where

import Beckn.Types.Id
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.CancellationReason as QCR
import qualified Types.API.CancellationReason as CancellationReasonAPI
import Utils.Common

list :: Id Person.Person -> FlowHandler CancellationReasonAPI.ListRes
list _ = withFlowHandlerAPI $ do
  fmap SCR.makeCancellationReasonAPIEntity <$> QCR.findAll
