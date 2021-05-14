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
  result <- Q.createFlow c
  checkDBError result

-- | Find Cases
findAllByTypeAndStatuses ::
  Id Person ->
  CaseType ->
  [CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  Flow [Case]
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset = do
  result <- Q.findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset
  checkDBError result

-- | Find Case by id
findById :: Id Case -> Flow Case
findById caseId = do
  result <- Q.findById caseId
  checkDBErrorOrEmpty result CaseNotFound

-- | Find Case by id and type
findByIdAndType :: Id Case -> CaseType -> Flow Case
findByIdAndType caseId caseType = do
  result <- Q.findByIdAndType caseId caseType
  checkDBErrorOrEmpty result CaseNotFound

-- | Find Case by id and a requestor id
findIdByPerson :: Person.Person -> Id Case -> Flow Case
findIdByPerson person caseId = do
  result <- Q.findIdByPerson person caseId
  checkDBErrorOrEmpty result CaseNotFound

-- | Find Cases by list of ids
findAllByIds :: [Id Case] -> Flow [Case]
findAllByIds caseIds =
  if null caseIds
    then pure []
    else do
      result <- Q.findAllByIds caseIds
      checkDBError result

-- | Find Cases by a requestor id
findAllByPerson :: Text -> Flow [Case]
findAllByPerson perId = do
  result <- Q.findAllByPerson perId
  checkDBError result

-- | Find Cases by status and expirtaion date
findAllExpiredByStatus :: [CaseStatus] -> Maybe UTCTime -> Maybe UTCTime -> Flow [Case]
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  result <- Q.findAllExpiredByStatus statuses maybeFrom maybeTo
  checkDBError result

-- | Update Case validity date
updateValidTill :: Id Case -> UTCTime -> Flow ()
updateValidTill cid validTill = do
  result <- Q.updateValidTillFlow cid validTill
  checkDBError result

-- | Validate and update Case status
updateStatus :: Id Case -> CaseStatus -> Flow ()
updateStatus cid status = do
  validateStatusChange status cid
  result <- Q.updateStatusFlow cid status
  checkDBError result

-- | Validate and update Case status and its udfs
updateStatusAndUdfs ::
  Id Case ->
  CaseStatus ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Flow ()
updateStatusAndUdfs cid status udf1 udf2 udf3 udf4 udf5 = do
  validateStatusChange status cid
  result <- Q.updateStatusAndUdfs cid status udf1 udf2 udf3 udf4 udf5
  checkDBError result

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
  result <- Q.findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset
  checkDBError result

-- | Get Case and validate its status change
validateStatusChange :: CaseStatus -> Id Case -> Flow ()
validateStatusChange newStatus caseId = do
  c <- findById caseId
  validateStatusTransition (_status c) newStatus
    & fromEitherM CaseInvalidStatus

updateInfo :: Id Case -> Text -> Flow ()
updateInfo cId info = do
  result <- Q.updateInfoFlow cId info
  checkDBError result
