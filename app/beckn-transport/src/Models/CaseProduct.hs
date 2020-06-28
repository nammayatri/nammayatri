module Models.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.CaseProduct
import Beckn.Utils.Common (fromDBError)
import qualified EulerHS.Language as L
import qualified Storage.Queries.CaseProduct as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

updateStatus :: ProductsId -> CaseProductStatus -> FlowResult ()
updateStatus id status = do
  result <- Q.updateStatus id status
  fromDBError result

updateAllCaseProductsByCaseId :: CaseId -> CaseProductStatus -> FlowResult ()
updateAllCaseProductsByCaseId caseId status = do
  result <- Q.updateAllCaseProductsByCaseId caseId status
  fromDBError result

updateStatusByIds ::
  [CaseProductId] ->
  Storage.CaseProductStatus ->
  L.Flow (T.DBResult ())

updateStatusForProducts :: ProductsId -> CaseProductStatus -> FlowResult ()
updateStatusForProducts productId status = do
  result <- Q.updateStatusForProducts caseId status
  fromDBError result
