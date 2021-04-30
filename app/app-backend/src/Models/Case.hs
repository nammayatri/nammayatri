module Models.Case where

import App.Types
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Person (Person)
import qualified Beckn.Types.Storage.Person as Person
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
create :: Case -> Flow ()
create c = do
  -- TODO add some validation checks
  -- and `throwDomainError CaseNotCreated` if needed
  Q.createFlow c

-- | Find Cases
findAllByTypeAndStatuses ::
  Id Person ->
  CaseType ->
  [CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  Flow [Case]
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset = do
  Q.findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset

-- | Find Case by id
findById :: Id Case -> Flow Case
findById caseId = do
  Q.findById caseId >>= fromMaybeM CaseNotFound

-- | Find Case by id and type
findByIdAndType :: Id Case -> CaseType -> Flow Case
findByIdAndType caseId caseType = do
  Q.findByIdAndType caseId caseType >>= fromMaybeM CaseNotFound

-- | Find Case by id and a requestor id
findIdByPerson :: Person.Person -> Id Case -> Flow Case
findIdByPerson person caseId = do
  Q.findIdByPerson person caseId >>= fromMaybeM CaseNotFound

-- | Find Cases by list of ids
findAllByIds :: [Id Case] -> Flow [Case]
findAllByIds caseIds =
  if null caseIds
    then pure []
    else do
      Q.findAllByIds caseIds

-- | Find Cases by a requestor id
findAllByPerson :: Text -> Flow [Case]
findAllByPerson perId = do
  Q.findAllByPerson perId

-- | Find Cases by status and expirtaion date
findAllExpiredByStatus :: [CaseStatus] -> Maybe UTCTime -> Maybe UTCTime -> Flow [Case]
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  Q.findAllExpiredByStatus statuses maybeFrom maybeTo

-- | Update Case validity date
updateValidTill :: Id Case -> UTCTime -> Flow ()
updateValidTill cid validTill = do
  Q.updateValidTillFlow cid validTill

-- | Validate and update Case status
updateStatus :: Id Case -> CaseStatus -> Flow ()
updateStatus cid status = do
  Q.updateStatusFlow cid status

-- | Find Cases by locations
findAllWithLimitOffsetWhere ::
  [Text] ->
  [Text] ->
  [CaseType] ->
  [CaseStatus] ->
  [Text] ->
  Maybe Int ->
  Maybe Int ->
  Flow [Case]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset = do
  Q.findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset

updateInfo :: Id Case -> Text -> Flow ()
updateInfo cId info = do
  Q.updateInfoFlow cId info
