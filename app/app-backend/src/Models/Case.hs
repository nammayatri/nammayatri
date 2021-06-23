module Models.Case where

import Beckn.Storage.DB.Config
import Beckn.Types.Id
import Beckn.Types.Storage.Case
import qualified Beckn.Types.Storage.Location as Loc
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
create :: DBFlow m r => Case -> m ()
create c = do
  -- TODO add some validation checks
  -- and `throwDomainError CaseNotCreated` if needed
  Q.createFlow c

-- | Find Cases
findAllByTypeAndStatuses ::
  DBFlow m r =>
  Id Person ->
  CaseType ->
  [CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  m [Case]
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset = do
  Q.findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset

-- | Find Case by id
findById :: DBFlow m r => Id Case -> m Case
findById caseId = do
  Q.findById caseId >>= fromMaybeM CaseNotFound

-- | Find Case by id and type
findByIdAndType :: DBFlow m r => Id Case -> CaseType -> m Case
findByIdAndType caseId caseType = do
  Q.findByIdAndType caseId caseType >>= fromMaybeM CaseNotFound

-- | Find Case by id and a requestor id
findIdByPerson :: DBFlow m r => Person.Person -> Id Case -> m Case
findIdByPerson person caseId = do
  Q.findIdByPerson person caseId >>= fromMaybeM CaseNotFound

-- | Find Cases by list of ids
findAllByIds :: DBFlow m r => [Id Case] -> m [Case]
findAllByIds caseIds =
  if null caseIds
    then pure []
    else do
      Q.findAllByIds caseIds

-- | Find Cases by a requestor id
findAllByPerson :: DBFlow m r => Text -> m [Case]
findAllByPerson perId = do
  Q.findAllByPerson perId

-- | Find Cases by status and expirtaion date
findAllExpiredByStatus :: DBFlow m r => [CaseStatus] -> Maybe UTCTime -> Maybe UTCTime -> m [Case]
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  Q.findAllExpiredByStatus statuses maybeFrom maybeTo

-- | Update Case validity date
updateValidTill :: DBFlow m r => Id Case -> UTCTime -> m ()
updateValidTill cid validTill = do
  Q.updateValidTillFlow cid validTill

-- | Validate and update Case status
updateStatus :: DBFlow m r => Id Case -> CaseStatus -> m ()
updateStatus cid status = do
  Q.updateStatusFlow cid status

-- | Find Cases by locations
findAllWithLimitOffsetWhere ::
  DBFlow m r =>
  [Id Loc.Location] ->
  [Id Loc.Location] ->
  [CaseType] ->
  [CaseStatus] ->
  [Text] ->
  Maybe Int ->
  Maybe Int ->
  m [Case]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset = do
  Q.findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset

updateInfo :: DBFlow m r => Id Case -> Text -> m ()
updateInfo cId info = do
  Q.updateInfoFlow cId info
