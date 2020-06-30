module Models.Case where

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

updateStatus :: CaseId -> CaseStatus -> FlowDomainResult ()
updateStatus id status = do
  validateStatusChange id status
  result <- Q.updateStatus id status
  fromDBError result

updateStatusAndUdfs :: CaseId -> CaseStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowDomainResult ()
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  validateStatusChange id status
  result <- Q.updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5
  fromDBError result

findById :: CaseId -> FlowDomainResult Case
findById caseId = do
  result <- Q.findById' caseId
  fromDBErrorOrEmpty (CaseErr CaseNotFound) result

validateStatusChange :: CaseId -> CaseStatus -> FlowDomainResult ()
validateStatusChange id newStatus = do
  case_ <- findById id
  pure $ case case_ of
    Left err -> Left err
    Right c -> case validateStatusTransition (_status c) newStatus of
      Left msg -> Left $ CaseErr $ CaseStatusTransitionErr msg
      _ -> Right ()
