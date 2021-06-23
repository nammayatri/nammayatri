module Models.ProductInstance where

import Beckn.Types.Id
import Beckn.Types.Storage.Case (Case)
import Beckn.Types.Storage.Organization (Organization)
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
updateStatus :: HasFlowDBEnv m r => Id ProductInstance -> ProductInstanceStatus -> m ()
updateStatus prodInstId newStatus = do
  Q.updateStatusFlow prodInstId newStatus

-- | Validate and update ProductInstances statusses
updateStatusByIds :: HasFlowDBEnv m r => [Id ProductInstance] -> ProductInstanceStatus -> m ()
updateStatusByIds ids status = do
  Q.updateStatusByIdsFlow ids status

-- | Find Product Instance by id
findById :: HasFlowDBEnv m r => Id ProductInstance -> m ProductInstance
findById caseProductId = do
  Q.findById' caseProductId >>= fromMaybeM PINotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: HasFlowDBEnv m r => Id Case -> m [ProductInstance]
findAllByCaseId caseId = do
  Q.findAllByCaseId' caseId

-- | Find Product Instances
findAllByIds :: HasFlowDBEnv m r => [Id ProductInstance] -> m [ProductInstance]
findAllByIds ids = do
  Q.findAllByIds' ids

findAllExpiredByStatus :: HasFlowDBEnv m r => [ProductInstanceStatus] -> UTCTime -> m [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  Q.findAllExpiredByStatus statuses expiryTime

-- | Get ProductInstance By OrganizationId groupBy status
getCountByStatus ::
  HasFlowDBEnv m r =>
  Id Organization ->
  ProductInstanceType ->
  m [(ProductInstanceStatus, Int)]
getCountByStatus orgId piType = do
  Q.getCountByStatus' orgId piType

findByStartTimeBuffer ::
  HasFlowDBEnv m r =>
  ProductInstanceType ->
  UTCTime ->
  NominalDiffTime ->
  [ProductInstanceStatus] ->
  m [ProductInstance]
findByStartTimeBuffer piType startTime buffer statuses = do
  Q.findByStartTimeBuffer piType startTime buffer statuses
