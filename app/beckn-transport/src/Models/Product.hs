module Models.Product where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Storage.Products
import Beckn.Utils.Common
import qualified Storage.Queries.Products as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Find Product by id
findById :: Id Products -> Flow Products
findById pid = do
  result <- Q.findById' pid
  checkDBErrorOrEmpty result CaseNotFound
