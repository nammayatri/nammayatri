module Models.Product where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Products
import Beckn.Utils.Common
import Control.Monad.Except
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Products as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- -- | Validate and update Product status
-- updateStatus :: ProductsId -> ProductsStatus -> FlowDomainResult ()
-- updateStatus id status = runExceptT $ do
--   validateStatusChange status id
--   ExceptT $ do
--     result <- Q.updateStatus id status
--     fromDBError result

-- -- | Validate status change and update product fields
-- updateMultiple :: ProductsId -> Products -> FlowDomainResult ()
-- updateMultiple id prd = runExceptT $ do
--   validateStatusChange (_status prd) id
--   ExceptT $ do
--     result <- Q.updateMultiple id prd
--     fromDBError result

-- | Find Product by id
findById :: ProductsId -> FlowDomainResult Products
findById id = do
  result <- Q.findById' id
  fromDBErrorOrEmpty (CaseErr CaseNotFound) result

-- -- | Get Product and validate its status change
-- validateStatusChange :: ProductsStatus -> ProductsId -> ExceptT DomainError L.Flow ()
-- validateStatusChange newStatus productId = do
--   c <- ExceptT $ findById productId
--   liftEither $ case validateStatusTransition (_status c) newStatus of
--     Left msg -> Left $ ProductErr $ ProductStatusTransitionErr msg
--     _ -> Right ()
