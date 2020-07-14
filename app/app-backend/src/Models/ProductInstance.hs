module Models.ProductInstance where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.ProductInstance
import Beckn.Utils.Common
import Control.Monad.Except
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Validate and update ProductInstance status
updateStatus :: ProductInstanceId -> ProductInstanceStatus -> FlowDomainResult ()
updateStatus id status = runExceptT $ do
  validatePIStatusChange status id
  ExceptT $ do
    result <- Q.updateStatus id status
    fromDBError result

-- | Bulk validate and update Case's ProductInstances statuses
updateAllProductInstancesByCaseId :: CaseId -> ProductInstanceStatus -> FlowDomainResult ()
updateAllProductInstancesByCaseId caseId status = runExceptT $ do
  validatePIStatusesChange status caseId
  ExceptT $ do
    result <- Q.updateAllProductInstByCaseId caseId status
    fromDBError result

updateMultiple :: ProductInstanceId -> ProductInstance -> FlowDomainResult ()
updateMultiple id prdInst = runExceptT $ do
  validatePIStatusChange (_status prdInst) id
  ExceptT $ do
    result <- Q.updateMultiple id prdInst
    fromDBError result

-- | Find Product Instance by id
findById :: ProductInstanceId -> FlowDomainResult ProductInstance
findById caseProductId = do
  result <- Q.findById' caseProductId
  fromDBErrorOrEmpty (ProductInstanceErr ProductInstanceNotFound) result

-- | Find Product Instances by Case Id
findAllByCaseId :: CaseId -> FlowDomainResult [ProductInstance]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId' caseId
  fromDBError result

-- | Find Product Instance by Product Id
findByProductId :: ProductsId -> FlowDomainResult ProductInstance
findByProductId pId = do
  result <- Q.findByProductId' pId
  fromDBErrorOrEmpty (ProductInstanceErr ProductInstanceNotFound) result

-- | Get ProductInstance and validate its status change
validatePIStatusChange :: ProductInstanceStatus -> ProductInstanceId -> ExceptT DomainError Flow ()
validatePIStatusChange newStatus productInstanceId = do
  cp <- ExceptT $ findById productInstanceId
  liftEither $ validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange :: ProductInstanceStatus -> CaseId -> ExceptT DomainError Flow ()
validatePIStatusesChange newStatus caseId = do
  cps <- ExceptT $ findAllByCaseId caseId
  validatePIStatusesChange' newStatus cps

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange' :: ProductInstanceStatus -> [ProductInstance] -> ExceptT DomainError Flow ()
validatePIStatusesChange' newStatus productInstances = do
  case mapM (validateStatusChange newStatus) productInstances of
    -- throwErrror, throwE is a shorthand for ExceptT . pure . Left
    Left err -> throwError err
    Right _ -> pure ()

-- | Validate status change and return appropriate DomainError
validateStatusChange :: ProductInstanceStatus -> ProductInstance -> DomainResult ()
validateStatusChange newStatus caseProduct =
  case validateStatusTransition (_status caseProduct) newStatus of
    Left msg -> Left $ ProductInstanceErr $ ProductInstanceStatusTransitionErr $ ErrorMsg msg
    _ -> Right ()
