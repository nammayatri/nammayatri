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

create :: Storage.Case -> Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= either DB.throwDBError pure

findAllByIds :: [CaseId] -> Flow [Storage.Case]
findAllByIds ids =
  DB.findAllOrErr dbTable (pred ids)
  where
    pred ids Storage.Case {..} =
      B.in_ _id (B.val_ <$> ids)

findById :: CaseId -> Flow Storage.Case
findById caseId =
  DB.findOneWithErr dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. B.val_ caseId

findByParentCaseIdAndType :: CaseId -> Storage.CaseType -> Flow (Maybe Storage.Case)
findByParentCaseIdAndType pCaseId cType =
  DB.findOne dbTable (predicate pCaseId cType)
    >>= either DB.throwDBError pure
  where
    predicate pCaseId cType Storage.Case {..} =
      _parentCaseId ==. B.val_ (Just pCaseId)
        &&. _type ==. B.val_ cType

findBySid :: Text -> Flow Storage.Case
findBySid sid =
  DB.findOneWithErr dbTable (predicate sid)
  where
    predicate sid Storage.Case {..} = _shortId ==. B.val_ sid

updateStatus ::
  CaseId ->
  Storage.CaseStatus ->
  Flow (T.DBResult ())
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Case {..} = _id ==. B.val_ id
    setClause status currTime Storage.Case {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
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

findByIdType :: [CaseId] -> Storage.CaseType -> Flow Storage.Case
findByIdType ids type_ =
  DB.findOneWithErr dbTable (predicate ids type_)
  where
    predicate ids type_ Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByIdType :: [CaseId] -> Storage.CaseType -> Flow [Storage.Case]
findAllByIdType ids type_ =
  DB.findAllOrErr dbTable (predicate ids type_)
  where
    predicate ids type_ Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ _id (B.val_ <$> ids)

findAllByTypeStatuses :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> [CaseId] -> LocalTime -> Flow [Storage.Case]
findAllByTypeStatuses limit offset csType statuses ignoreIds now =
  DB.findAllWithLimitOffsetWhere dbTable (predicate csType statuses ignoreIds now) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate csType statuses ignoreIds now Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. B.not_ (B.in_ _id (B.val_ <$> ignoreIds))
        &&. _validTill B.>. B.val_ now

findAllByTypeStatusTime :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> [CaseId] -> LocalTime -> LocalTime -> Flow [Storage.Case]
findAllByTypeStatusTime limit offset csType statuses ignoreIds now fromTime =
  DB.findAllWithLimitOffsetWhere dbTable (predicate csType statuses ignoreIds now fromTime) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate csType statuses ignoreIds now fromTime Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. B.not_ (B.in_ _id (B.val_ <$> ignoreIds))
        &&. _validTill B.>. B.val_ now
        &&. _createdAt B.<. B.val_ fromTime

findAllExpiredByStatus :: [Storage.CaseStatus] -> Storage.CaseType -> LocalTime -> LocalTime -> Flow [Storage.Case]
findAllExpiredByStatus statuses csType from to = do
  (now :: LocalTime) <- getCurrentTimeUTC
  DB.findAll dbTable (predicate now from to)
    >>= either DB.throwDBError pure
  where
    predicate now from to Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ _status (B.val_ <$> statuses)
        &&. _validTill B.<=. B.val_ now
        &&. _createdAt B.>=. B.val_ from
        &&. _createdAt B.<=. B.val_ to
