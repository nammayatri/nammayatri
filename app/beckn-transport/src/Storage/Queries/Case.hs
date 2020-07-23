module Storage.Queries.Case where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.transporterDb

create :: Storage.Case -> Flow (T.DBResult ())
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})

findAllByIds :: [CaseId] -> Flow (T.DBResult [Storage.Case])
findAllByIds ids =
  DB.findAll dbTable (pred ids)
  where
    pred ids Storage.Case {..} =
      B.in_ _id (B.val_ <$> ids)

findById :: CaseId -> Flow (T.DBResult (Maybe Storage.Case))
findById caseId =
  DB.findOne dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. B.val_ caseId

findByParentCaseIdAndType :: CaseId -> Storage.CaseType -> Flow (T.DBResult (Maybe Storage.Case))
findByParentCaseIdAndType pCaseId cType =
  DB.findOne dbTable (predicate pCaseId cType)
  where
    predicate pCaseId cType Storage.Case {..} =
      _parentCaseId ==. B.val_ (Just pCaseId)
        &&. _type ==. B.val_ cType

findBySid :: Text -> Flow (T.DBResult (Maybe Storage.Case))
findBySid sid =
  DB.findOne dbTable (predicate sid)
  where
    predicate sid Storage.Case {..} = _shortId ==. B.val_ sid

updateStatus ::
  CaseId ->
  Storage.CaseStatus ->
  Flow (T.DBResult ())
updateStatus id newStatus = do
  -- update data
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause newStatus currTime)
    (predicate id)
  where
    predicate id Storage.Case {..} = _id ==. B.val_ id
    setClause newStatus currTime Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ newStatus
        ]

updateStatusByIds ::
  [CaseId] ->
  Storage.CaseStatus ->
  Flow (T.DBResult ())
updateStatusByIds ids status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate ids)
  where
    predicate ids Storage.Case {..} = B.in_ _id (B.val_ <$> ids)
    setClause status currTime Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

findByIdType :: [CaseId] -> Storage.CaseType -> Flow (T.DBResult (Maybe Storage.Case))
findByIdType ids type_ =
  DB.findOne dbTable (predicate ids type_)
  where
    predicate ids type_ Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByIdType :: [CaseId] -> Storage.CaseType -> Flow (T.DBResult [Storage.Case])
findAllByIdType ids type_ =
  DB.findAll dbTable (predicate ids type_)
  where
    predicate ids type_ Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByTypeStatuses :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> [CaseId] -> LocalTime -> Flow (T.DBResult [Storage.Case])
findAllByTypeStatuses limit offset csType statuses ignoreIds now =
  DB.findAllWithLimitOffsetWhere dbTable (predicate csType statuses ignoreIds now) limit offset orderByDesc
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate csType statuses ignoreIds now Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. B.not_ (B.in_ _id (B.val_ <$> ignoreIds))
        &&. _validTill B.>. B.val_ now

findAllByTypeStatusTime :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> [CaseId] -> LocalTime -> LocalTime -> Flow (T.DBResult [Storage.Case])
findAllByTypeStatusTime limit offset csType statuses ignoreIds now fromTime =
  DB.findAllWithLimitOffsetWhere dbTable (predicate csType statuses ignoreIds now fromTime) limit offset orderByDesc
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate csType statuses ignoreIds now fromTime Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. B.not_ (B.in_ _id (B.val_ <$> ignoreIds))
        &&. _validTill B.>. B.val_ now
        &&. _createdAt B.<. B.val_ fromTime

findAllExpiredByStatus :: [Storage.CaseStatus] -> Storage.CaseType -> LocalTime -> LocalTime -> Flow (T.DBResult [Storage.Case])
findAllExpiredByStatus statuses csType from to = do
  (now :: LocalTime) <- getCurrentTimeUTC
  DB.findAll dbTable (predicate now from to)
  where
    predicate now from to Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. _validTill B.<=. B.val_ now
        &&. _createdAt B.>=. B.val_ from
        &&. _createdAt B.<=. B.val_ to
