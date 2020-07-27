module Models.Product where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Products
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Products as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Find Product by id
findById :: ProductsId -> Flow Products
findById id = do
  result <- Q.findById' id
  checkDBErrorOrEmpty result $ CaseErr CaseNotFound
