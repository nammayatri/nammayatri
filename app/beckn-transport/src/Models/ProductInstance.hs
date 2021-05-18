module Models.ProductInstance where

import App.Types
import Beckn.Types.Id
import Beckn.Types.Storage.Case (Case)
import Beckn.Types.Storage.ProductInstance
import Data.Time (NominalDiffTime, UTCTime)
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as Q
import Types.Error
import Utils.Common

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Validate and update ProductInstance status
updateStatus :: Id ProductInstance -> ProductInstanceStatus -> Flow ()
updateStatus prodInstId newStatus = do
  result <- Q.updateStatusFlow prodInstId newStatus
  checkDBError result

-- | Validate and update ProductInstances statusses
updateStatusByIds :: [Id ProductInstance] -> ProductInstanceStatus -> Flow ()
updateStatusByIds ids status = do
  result <- Q.updateStatusByIdsFlow ids status
  checkDBError result

-- | Find Product Instance by id
findById :: Id ProductInstance -> Flow ProductInstance
findById caseProductId = do
  result <- Q.findById' caseProductId
  checkDBErrorOrEmpty result PINotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: Id Case -> Flow [ProductInstance]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId' caseId
  checkDBError result

-- | Find Product Instances
findAllByIds :: [Id ProductInstance] -> Flow [ProductInstance]
findAllByIds ids = do
  result <- Q.findAllByIds' ids
  checkDBError result

findAllExpiredByStatus :: [ProductInstanceStatus] -> UTCTime -> Flow [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  result <- Q.findAllExpiredByStatus statuses expiryTime
  checkDBError result

-- | Get ProductInstance By OrganizationId groupBy status
getCountByStatus :: Text -> ProductInstanceType -> Flow [(ProductInstanceStatus, Int)]
getCountByStatus orgId piType = do
  result <- Q.getCountByStatus' orgId piType
  checkDBError result

findByStartTimeBuffer :: ProductInstanceType -> UTCTime -> NominalDiffTime -> [ProductInstanceStatus] -> Flow [ProductInstance]
findByStartTimeBuffer piType startTime buffer statuses = do
  result <- Q.findByStartTimeBuffer piType startTime buffer statuses
  checkDBError result
