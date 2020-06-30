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

updateStatus :: CaseId -> ProductsId -> CaseProductStatus -> FlowDomainResult ()
updateStatus caseId productId newStatus = do
  result <- Q.updateStatus caseId productId newStatus
  fromDBError result

updateStatusByIds :: [CaseProductId] -> CaseProductStatus -> FlowDomainResult ()
updateStatusByIds ids status = do
  result <- Q.updateStatusByIds ids status
  fromDBError result
