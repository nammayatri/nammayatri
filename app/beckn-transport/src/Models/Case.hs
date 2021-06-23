module Models.Case where

import Beckn.Types.Id
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization (Organization)
import Data.Maybe
import Data.Text
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.Case as Q
import Types.Error
import Utils.Common

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Create Case
create :: HasFlowDBEnv m r => Case -> m ()
create c = do
  -- TODO add some validation checks
  -- and `throwDomainError CaseNotCreated` if needed
  Q.createFlow c

-- | Find Case by id
findById :: HasFlowDBEnv m r => Id Case -> m Case
findById caseId = do
  Q.findById caseId >>= fromMaybeM CaseNotFound

-- | Find Cases by id list
findAllByIds :: HasFlowDBEnv m r => [Id Case] -> m [Case]
findAllByIds ids = do
  Q.findAllByIds ids

-- | Find Case by parent case id and type
findByParentCaseIdAndType :: HasFlowDBEnv m r => Id Case -> CaseType -> m (Maybe Case)
findByParentCaseIdAndType pCaseId cType = do
  Q.findByParentCaseIdAndType pCaseId cType

-- | Find Case by short id
findBySid :: HasFlowDBEnv m r => Text -> m Case
findBySid sid = do
  Q.findBySid sid >>= fromMaybeM CaseNotFound

-- | Validate and update Case status
updateStatus :: HasFlowDBEnv m r => Id Case -> CaseStatus -> m ()
updateStatus cid status = do
  Q.updateStatusFlow cid status

-- | Validate and update Cases statuses
updateStatusByIds :: HasFlowDBEnv m r => [Id Case] -> CaseStatus -> m ()
updateStatusByIds ids status = do
  Q.updateStatusByIdsFlow ids status

-- | Find Case by id and type
findByIdType :: HasFlowDBEnv m r => [Id Case] -> CaseType -> m Case
findByIdType ids type_ = do
  Q.findByIdType ids type_ >>= fromMaybeM CaseNotFound

-- | Find Cases by id and type
findAllByIdType :: HasFlowDBEnv m r => [Id Case] -> CaseType -> m [Case]
findAllByIdType ids type_ = do
  Q.findAllByIdType ids type_

-- | Find Cases
findAllByTypeStatuses ::
  HasFlowDBEnv m r =>
  Integer ->
  Integer ->
  CaseType ->
  [CaseStatus] ->
  Id Organization ->
  UTCTime ->
  m [Case]
findAllByTypeStatuses limit offset csType statuses orgId now = do
  Q.findAllByTypeStatuses limit offset csType statuses orgId now

-- | Find Cases
findAllByTypeStatusTime ::
  HasFlowDBEnv m r =>
  Integer ->
  Integer ->
  CaseType ->
  [CaseStatus] ->
  Id Organization ->
  UTCTime ->
  UTCTime ->
  m [Case]
findAllByTypeStatusTime limit offset csType statuses orgId now fromTime = do
  Q.findAllByTypeStatusTime limit offset csType statuses orgId now fromTime

-- | Find Cases by status and expirtaion date
findAllExpiredByStatus :: HasFlowDBEnv m r => [CaseStatus] -> CaseType -> UTCTime -> UTCTime -> m [Case]
findAllExpiredByStatus statuses csType from to = do
  Q.findAllExpiredByStatus statuses csType from to
