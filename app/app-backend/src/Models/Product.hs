module Models.Product where

import Beckn.Types.Id
import Beckn.Types.Storage.Products
import EulerHS.Prelude
import qualified Storage.Queries.Products as Q
import Types.Error
import Utils.Common

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

create :: HasFlowDBEnv m r => Products -> m ()
create = Q.createFlow

-- | Find Product by id
findById :: HasFlowDBEnv m r => Id Products -> m Products
findById pid = do
  Q.findById pid >>= fromMaybeM CaseNotFound

findAllByIds :: HasFlowDBEnv m r => [Id Products] -> m [Products]
findAllByIds pids = do
  Q.findAllByIds pids
