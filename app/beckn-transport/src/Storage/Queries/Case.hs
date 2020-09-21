module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.transporterDb <$> getSchemaName

create :: Storage.Case -> Flow (T.DBResult ())
create Storage.Case {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})

findAllByIds :: [CaseId] -> Flow (T.DBResult [Storage.Case])
findAllByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.Case {..} =
      B.in_ _id (B.val_ <$> ids)

findById :: CaseId -> Flow (T.DBResult (Maybe Storage.Case))
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = _id ==. B.val_ caseId

findByParentCaseIdAndType :: CaseId -> Storage.CaseType -> Flow (T.DBResult (Maybe Storage.Case))
findByParentCaseIdAndType pCaseId cType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      _parentCaseId ==. B.val_ (Just pCaseId)
        &&. _type ==. B.val_ cType

findBySid :: Text -> Flow (T.DBResult (Maybe Storage.Case))
findBySid sid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = _shortId ==. B.val_ sid

updateStatus ::
  CaseId ->
  Storage.CaseStatus ->
  Flow (T.DBResult ())
updateStatus id newStatus = do
  dbTable <- getDbTable
  -- update data
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause newStatus currTime)
    (predicate id)
  where
    predicate cid Storage.Case {..} = _id ==. B.val_ cid
    setClause status currTime Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateStatusByIds ::
  [CaseId] ->
  Storage.CaseStatus ->
  Flow (T.DBResult ())
updateStatusByIds ids newStatus = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause newStatus currTime)
    (predicate ids)
  where
    predicate cids Storage.Case {..} = B.in_ _id (B.val_ <$> cids)
    setClause status currTime Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

findByIdType :: [CaseId] -> Storage.CaseType -> Flow (T.DBResult (Maybe Storage.Case))
findByIdType ids type_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByIdType :: [CaseId] -> Storage.CaseType -> Flow (T.DBResult [Storage.Case])
findAllByIdType ids type_ = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
  where
    predicate Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByTypeStatuses :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> Text -> UTCTime -> Flow (T.DBResult [Storage.Case])
findAllByTypeStatuses limit offset csType statuses orgId now = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate Storage.Case {..} =
      _type ==. B.val_ csType
        &&. _provider ==. B.val_ (Just orgId)
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. _validTill B.>. B.val_ now

findAllByTypeStatusTime :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> Text -> UTCTime -> UTCTime -> Flow (T.DBResult [Storage.Case])
findAllByTypeStatusTime limit offset csType statuses orgId now fromTime = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate Storage.Case {..} =
      _type ==. B.val_ csType
        &&. _provider ==. B.val_ (Just orgId)
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. _validTill B.>. B.val_ now
        &&. _createdAt B.<. B.val_ fromTime

findAllExpiredByStatus :: [Storage.CaseStatus] -> Storage.CaseType -> UTCTime -> UTCTime -> Flow (T.DBResult [Storage.Case])
findAllExpiredByStatus statuses csType from to = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrTime
  DB.findAll dbTable (predicate now)
  where
    predicate now Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. _validTill B.<=. B.val_ now
        &&. _createdAt B.>=. B.val_ from
        &&. _createdAt B.<=. B.val_ to
