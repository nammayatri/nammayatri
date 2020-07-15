module Models.Case where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Case
import Beckn.Utils.Common
import Data.Maybe
import Data.Text
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Case as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

-- | Validate and update Case status
updateStatus :: CaseId -> CaseStatus -> Flow ()
updateStatus id status = do
  validateCaseStatuseChange status id
  result <- Q.updateStatus id status
  checkDBError result

updateStatusByIds :: [CaseId] -> CaseStatus -> Flow ()
updateStatusByIds ids status = do
  cases <- findAllByIds ids
  validateCasesStatusesChange' status cases
  result <- Q.updateStatusByIds ids status
  checkDBError result

-- Queries wrappers
findById :: CaseId -> Flow Case
findById caseId = do
  result <- Q.findById' caseId
  checkDBErrorOrEmpty (CaseErr CaseNotFound) result

-- | Find Product Instances
findAllByIds :: [CaseId] -> Flow [Case]
findAllByIds ids = do
  result <- Q.findAllByIds' ids
  checkDBError result

-- | Get Case and validate its status change
validateCaseStatuseChange :: CaseStatus -> CaseId -> Flow ()
validateCaseStatuseChange newStatus caseId = do
  case_ <- findById caseId
  validateStatusChange newStatus case_

-- | Bulk validation of Case statuses change
validateCasesStatusesChange :: CaseStatus -> [CaseId] -> Flow ()
validateCasesStatusesChange newStatus caseIds = do
  cps <- findAllByIds caseIds
  validateCasesStatusesChange' newStatus cps

-- | Bulk validation of Case statuses change
validateCasesStatusesChange' :: CaseStatus -> [Case] -> Flow ()
validateCasesStatusesChange' newStatus =
  mapM_ (validateStatusChange newStatus)

-- | Get Case and validate its status change
validateStatusChange :: CaseStatus -> Case -> Flow ()
validateStatusChange newStatus case_ = do
  case validateStatusTransition (_status case_) newStatus of
    Left msg -> throwDomainError $ CaseErr $ CaseStatusTransitionErr $ ErrorMsg msg
    _ -> pure ()
