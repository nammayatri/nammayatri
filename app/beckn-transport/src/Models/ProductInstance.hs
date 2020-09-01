module Models.ProductInstance where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.ProductInstance
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Validate and update ProductInstance status
updateStatus :: ProductInstanceId -> ProductInstanceStatus -> Flow ()
updateStatus prodInstId newStatus = do
  validatePIStatusChange newStatus prodInstId
  result <- Q.updateStatus prodInstId newStatus
  checkDBError result

-- | Validate and update ProductInstances statusses
updateStatusByIds :: [ProductInstanceId] -> ProductInstanceStatus -> Flow ()
updateStatusByIds ids status = do
  productInstances <- findAllByIds ids
  validatePIStatusesChange' status productInstances
  result <- Q.updateStatusByIds ids status
  checkDBError result

-- | Find Product Instance by id
findById :: ProductInstanceId -> Flow ProductInstance
findById caseProductId = do
  result <- Q.findById' caseProductId
  checkDBErrorOrEmpty result $ ProductInstanceErr ProductInstanceNotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: CaseId -> Flow [ProductInstance]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId' caseId
  checkDBError result

-- | Find Product Instances
findAllByIds :: [ProductInstanceId] -> Flow [ProductInstance]
findAllByIds ids = do
  result <- Q.findAllByIds' ids
  checkDBError result

-- | Get ProductInstance and validate its status change
validatePIStatusChange :: ProductInstanceStatus -> ProductInstanceId -> Flow ()
validatePIStatusChange newStatus productInstanceId = do
  cp <- findById productInstanceId
  validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange' :: ProductInstanceStatus -> [ProductInstance] -> Flow ()
validatePIStatusesChange' newStatus =
  mapM_ (validateStatusChange newStatus)

-- | Validate status change and return appropriate DomainError
validateStatusChange :: ProductInstanceStatus -> ProductInstance -> Flow ()
validateStatusChange newStatus productInstance =
  case validateStatusTransition (_status productInstance) newStatus of
    Left msg -> throwDomainError $ ProductInstanceErr $ ProductInstanceStatusTransitionErr $ ErrorMsg msg
    _ -> pure ()

findAllExpiredByStatus :: [ProductInstanceStatus] -> UTCTime -> Flow [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  result <- Q.findAllExpiredByStatus statuses expiryTime
  checkDBError result

-- | Get ProductInstance By OrganizationId groupBy status
getCountByStatus :: Text -> ProductInstanceType -> Flow [(ProductInstanceStatus, Int)]
getCountByStatus orgId piType = do
  result <- Q.getCountByStatus' orgId piType
  checkDBError result
