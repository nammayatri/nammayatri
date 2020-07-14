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
updateStatus prodInstId newStatus = runExceptT $do
  validatePIStatusChange newStatus prodInstId
  ExceptT $ do
    result <- Q.updateStatus prodInstId newStatus
    fromDBError result

-- | Validate and update ProductInstances statusses
updateStatusByIds :: [ProductInstanceId] -> ProductInstanceStatus -> FlowDomainResult ()
updateStatusByIds ids status = runExceptT $ do
  productInstances <- ExceptT $ findAllByIds ids
  validatePIStatusesChange' status productInstances
  ExceptT $ do
    result <- Q.updateStatusByIds ids status
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

-- | Find Product Instances
findAllByIds :: [ProductInstanceId] -> FlowDomainResult [ProductInstance]
findAllByIds ids = do
  result <- Q.findAllByIds' ids
  fromDBError result

-- | Get ProductInstance and validate its status change
validatePIStatusChange :: ProductInstanceStatus -> ProductInstanceId -> ExceptT DomainError Flow ()
validatePIStatusChange newStatus productInstanceId = do
  cp <- ExceptT $ findById productInstanceId
  liftEither $ validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange' :: ProductInstanceStatus -> [ProductInstance] -> ExceptT DomainError Flow ()
validatePIStatusesChange' newStatus cps = do
  case mapM (validateStatusChange newStatus) cps of
    -- throwErrror, throwE is a shorthand for ExceptT . pure . Left
    Left err -> throwError err
    Right _ -> pure ()

-- | Validate status change and return appropriate DomainError
validateStatusChange :: ProductInstanceStatus -> ProductInstance -> DomainResult ()
validateStatusChange newStatus productInstance =
  case validateStatusTransition (_status productInstance) newStatus of
    Left msg -> Left $ ProductInstanceErr $ ProductInstanceStatusTransitionErr $ ErrorMsg msg
    _ -> Right ()
