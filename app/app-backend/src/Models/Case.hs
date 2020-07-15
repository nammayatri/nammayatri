module Models.Case where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Case
import Beckn.Utils.Common
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
  validateStatusChange status id
  result <- Q.updateStatus id status
  checkDBError result

-- | Validate and update Case status and its udfs
updateStatusAndUdfs ::
  CaseId ->
  CaseStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow ()
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  validateStatusChange status id
  result <- Q.updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5
  checkDBError result

-- Queries wrappers
findById :: CaseId -> Flow Case
findById caseId = do
  result <- Q.findById' caseId
  checkDBErrorOrEmpty (CaseErr CaseNotFound) result

-- | Get Case and validate its status change
validateStatusChange :: CaseStatus -> CaseId -> Flow ()
validateStatusChange newStatus caseId = do
  c <- findById caseId
  case validateStatusTransition (_status c) newStatus of
    Left msg -> L.throwException $ CaseErr $ CaseStatusTransitionErr $ ErrorMsg msg
    _ -> pure ()
