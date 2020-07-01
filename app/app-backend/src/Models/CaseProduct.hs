module Models.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.CaseProduct
import Beckn.Utils.Common
import Control.Monad.Except
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.CaseProduct as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Validate and update ProductInstance status
updateStatus :: ProductsId -> CaseProductStatus -> FlowDomainResult ()
updateStatus id status = runExceptT $ do
  validateCPStatusChange status id
  ExceptT $ do
    result <- Q.updateStatus id status
    fromDBError result

-- | Bulk validate and update Case's ProductInstances statuses
updateAllCaseProductsByCaseId :: CaseId -> CaseProductStatus -> FlowDomainResult ()
updateAllCaseProductsByCaseId caseId status = runExceptT $ do
  validateCPSStatusesChange status caseId
  ExceptT $ do
    result <- Q.updateAllCaseProductsByCaseId caseId status
    fromDBError result

-- | Find Product Instance by id
findById :: CaseProductId -> FlowDomainResult CaseProduct
findById caseProductId = do
  result <- Q.findById' caseProductId
  fromDBErrorOrEmpty (ProductInstanceErr ProductInstanceNotFound) result

-- | Find Product Instances by Case Id
findAllByCaseId :: CaseId -> FlowDomainResult [CaseProduct]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId' caseId
  fromDBError result

-- | Find Product Instance by Product Id
findByProductId :: ProductsId -> FlowDomainResult CaseProduct
findByProductId pId = do
  result <- Q.findByProductId' pId
  fromDBErrorOrEmpty (ProductInstanceErr ProductInstanceNotFound) result

-- | Get ProductInstance and validate its status change
validateCPStatusChange :: CaseProductStatus -> ProductsId -> ExceptT DomainError L.Flow ()
validateCPStatusChange newStatus caseId = do
  cp <- ExceptT $ findByProductId caseId
  liftEither $ validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validateCPSStatusesChange :: CaseProductStatus -> CaseId -> ExceptT DomainError L.Flow ()
validateCPSStatusesChange newStatus caseId = do
  cps <- ExceptT $ findAllByCaseId caseId
  validateCPSStatusesChange' newStatus cps

-- | Bulk validation of ProductInstance statuses change
validateCPSStatusesChange' :: CaseProductStatus -> [CaseProduct] -> ExceptT DomainError L.Flow ()
validateCPSStatusesChange' newStatus cps = do
  case sequence $ fmap (validateStatusChange newStatus) cps of
    -- throwErrror, throwE is a shorthand for ExceptT . pure . Left
    Left err -> throwError err
    Right _ -> pure ()

-- | Validate status change and return appropriate DomainError
validateStatusChange :: CaseProductStatus -> CaseProduct -> DomainResult ()
validateStatusChange newStatus caseProduct =
  case validateStatusTransition (_status caseProduct) newStatus of
    Left msg -> Left $ ProductInstanceErr $ ProductInstanceStatusTransitionErr msg
    _ -> Right ()
