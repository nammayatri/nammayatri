module Models.Case where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Case
import Beckn.Utils.Common
import Control.Monad.Except
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
updateStatus :: CaseId -> CaseStatus -> FlowDomainResult ()
updateStatus id status = runExceptT $ do
  validateCaseStatuseChange status id
  ExceptT $ do
    result <- Q.updateStatus id status
    fromDBError result

updateStatusByIds :: [CaseId] -> CaseStatus -> FlowDomainResult ()
updateStatusByIds ids status = runExceptT $ do
  cases <- ExceptT $ findAllByIds ids
  validateCasesStatusesChange' status cases
  ExceptT $ do
    result <- Q.updateStatusByIds ids status
    fromDBError result

-- Queries wrappers
findById :: CaseId -> FlowDomainResult Case
findById caseId = do
  result <- Q.findById' caseId
  fromDBErrorOrEmpty (CaseErr CaseNotFound) result

-- | Find Product Instances
findAllByIds :: [CaseId] -> FlowDomainResult [Case]
findAllByIds ids = do
  result <- Q.findAllByIds' ids
  fromDBError result

-- | Get Case and validate its status change
validateCaseStatuseChange :: CaseStatus -> CaseId -> ExceptT DomainError Flow ()
validateCaseStatuseChange newStatus caseId = do
  case_ <- ExceptT $ findById caseId
  liftEither $ validateStatusChange newStatus case_

-- | Bulk validation of Case statuses change
validateCasesStatusesChange :: CaseStatus -> [CaseId] -> ExceptT DomainError Flow ()
validateCasesStatusesChange newStatus caseIds = do
  cps <- ExceptT $ findAllByIds caseIds
  validateCasesStatusesChange' newStatus cps

-- | Bulk validation of Case statuses change
validateCasesStatusesChange' :: CaseStatus -> [Case] -> ExceptT DomainError Flow ()
validateCasesStatusesChange' newStatus cases = do
  case mapM (validateStatusChange newStatus) cases of
    -- throwErrror, throwE is a shorthand for ExceptT . pure . Left
    Left err -> throwError err
    Right _ -> pure ()

-- | Get Case and validate its status change
validateStatusChange :: CaseStatus -> Case -> DomainResult ()
validateStatusChange newStatus case_ = do
  liftEither $ case validateStatusTransition (_status case_) newStatus of
    Left msg -> Left $ CaseErr $ CaseStatusTransitionErr $ ErrorMsg msg
    _ -> Right ()
