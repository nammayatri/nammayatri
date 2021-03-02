module Models.Product where

import App.Types
import Beckn.Types.Error
import Beckn.Types.ID
import Beckn.Types.Storage.Products
import Beckn.Utils.Common
import EulerHS.Prelude hiding (product)
import qualified Storage.Queries.Products as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

create :: Products -> Flow ()
create product = do
  result <- Q.create product
  checkDBError result

-- | Find Product by id
findById :: ID Products -> Flow Products
findById pid = do
  result <- Q.findById pid
  checkDBErrorOrEmpty result $ CaseErr CaseNotFound

findAllByIds :: [ID Products] -> Flow [Products]
findAllByIds pids = do
  result <- Q.findAllByIds pids
  checkDBError result
