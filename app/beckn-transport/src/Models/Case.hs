module Models.Case where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import Beckn.Utils.Common
import Data.Maybe
import Data.Text
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.Case as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Create Case
create :: Case -> Flow ()
create c = do
  -- TODO add some validation checks
  -- and `throwDomainError CaseNotCreated` if needed
  result <- Q.create c
  checkDBError result

-- | Find Case by id
findById :: Id Case -> Flow Case
findById caseId = do
  result <- Q.findById caseId
  checkDBErrorOrEmpty result $ CaseErr CaseNotFound

-- | Find Cases by id list
findAllByIds :: [Id Case] -> Flow [Case]
findAllByIds ids = do
  result <- Q.findAllByIds ids
  checkDBError result

-- | Find Case by parent case id and type
findByParentCaseIdAndType :: Id Case -> CaseType -> Flow (Maybe Case)
findByParentCaseIdAndType pCaseId cType = do
  result <- Q.findByParentCaseIdAndType pCaseId cType
  checkDBError result

-- | Find Case by short id
findBySid :: Text -> Flow Case
findBySid sid = do
  result <- Q.findBySid sid
  checkDBErrorOrEmpty result $ CaseErr CaseNotFound

-- | Validate and update Case status
updateStatus :: Id Case -> CaseStatus -> Flow ()
updateStatus cid status = do
  validateCaseStatuseChange status cid
  result <- Q.updateStatus cid status
  checkDBError result

-- | Validate and update Cases statuses
updateStatusByIds :: [Id Case] -> CaseStatus -> Flow ()
updateStatusByIds ids status = do
  cases <- findAllByIds ids
  validateCasesStatusesChange' status cases
  result <- Q.updateStatusByIds ids status
  checkDBError result

-- | Find Case by id and type
findByIdType :: [Id Case] -> CaseType -> Flow Case
findByIdType ids type_ = do
  result <- Q.findByIdType ids type_
  checkDBErrorOrEmpty result (CaseErr CaseNotFound)

-- | Find Cases by id and type
findAllByIdType :: [Id Case] -> CaseType -> Flow [Case]
findAllByIdType ids type_ = do
  result <- Q.findAllByIdType ids type_
  checkDBError result

-- | Find Cases
findAllByTypeStatuses ::
  Integer ->
  Integer ->
  CaseType ->
  [CaseStatus] ->
  Text ->
  UTCTime ->
  Flow [Case]
findAllByTypeStatuses limit offset csType statuses orgId now = do
  result <- Q.findAllByTypeStatuses limit offset csType statuses orgId now
  checkDBError result

-- | Find Cases
findAllByTypeStatusTime ::
  Integer ->
  Integer ->
  CaseType ->
  [CaseStatus] ->
  Text ->
  UTCTime ->
  UTCTime ->
  Flow [Case]
findAllByTypeStatusTime limit offset csType statuses orgId now fromTime = do
  result <- Q.findAllByTypeStatusTime limit offset csType statuses orgId now fromTime
  checkDBError result

-- | Find Cases by status and expirtaion date
findAllExpiredByStatus :: [CaseStatus] -> CaseType -> UTCTime -> UTCTime -> Flow [Case]
findAllExpiredByStatus statuses csType from to = do
  result <- Q.findAllExpiredByStatus statuses csType from to
  checkDBError result

-- | Get Case and validate its status change
validateCaseStatuseChange :: CaseStatus -> Id Case -> Flow ()
validateCaseStatuseChange newStatus caseId = do
  case_ <- findById caseId
  validateStatusChange newStatus case_

-- | Bulk validation of Case statuses change
validateCasesStatusesChange :: CaseStatus -> [Id Case] -> Flow ()
validateCasesStatusesChange newStatus caseIds = do
  cps <- findAllByIds caseIds
  validateCasesStatusesChange' newStatus cps

-- | Bulk validation of Case statuses change
validateCasesStatusesChange' :: CaseStatus -> [Case] -> Flow ()
validateCasesStatusesChange' newStatus =
  mapM_ (validateStatusChange newStatus)

-- | Get Case and validate its status change
validateStatusChange :: CaseStatus -> Case -> Flow ()
validateStatusChange newStatus case_ =
  case validateStatusTransition (_status case_) newStatus of
    Left msg -> throwDomainError $ CaseErr $ CaseStatusTransitionErr $ ErrorMsg msg
    _ -> pure ()
