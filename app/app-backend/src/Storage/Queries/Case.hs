{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Case as Storage
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.appDb <$> getSchemaName

createFlow :: Storage.Case -> Flow ()
createFlow = DB.runSqlDB . create

create :: Storage.Case -> DB.SqlDB ()
create case_ = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression case_)

findAllByTypeAndStatuses ::
  Id Person.Person ->
  Storage.CaseType ->
  [Storage.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  Flow [Storage.Case]
findAllByTypeAndStatuses personId caseType caseStatuses mlimit moffset = do
  dbTable <- getDbTable
  let limit = fromMaybe 100 mlimit
      offset = fromMaybe 0 moffset
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _type ==. B.val_ caseType,
          B.in_ status (B.val_ <$> caseStatuses) ||. complementVal caseStatuses,
          requestor ==. B.val_ (Just $ getId personId)
        ]

findById :: Id Storage.Case -> Flow (Maybe Storage.Case)
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = id ==. B.val_ caseId

findByIdAndType :: Id Storage.Case -> Storage.CaseType -> Flow (Maybe Storage.Case)
findByIdAndType caseId caseType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      (id ==. B.val_ caseId)
        &&. (_type ==. B.val_ caseType)

findIdByPerson :: Person.Person -> Id Storage.Case -> Flow (Maybe Storage.Case)
findIdByPerson person caseId = do
  dbTable <- getDbTable
  let personId = getId $ person ^. #id
  DB.findOne dbTable (predicate personId)
  where
    predicate personId Storage.Case {..} =
      id ==. B.val_ caseId &&. requestor ==. B.val_ (Just personId)

findAllByIds :: [Id Storage.Case] -> Flow [Storage.Case]
findAllByIds caseIds = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} = id `B.in_` (B.val_ <$> caseIds)

findAllByParentIdsAndCaseType :: [Id Storage.Case] -> Storage.CaseType -> Flow [Storage.Case]
findAllByParentIdsAndCaseType caseIds caseType = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} = parentCaseId `B.in_` (B.val_ . Just <$> caseIds) &&. (_type ==. B.val_ caseType)

findOneByParentIdAndCaseType :: Id Storage.Case -> Storage.CaseType -> Flow (Maybe Storage.Case)
findOneByParentIdAndCaseType caseId caseType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = parentCaseId ==. B.val_ (Just caseId) &&. _type ==. B.val_ caseType

findAllByPerson :: Text -> Flow [Storage.Case]
findAllByPerson perId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} = requestor ==. B.val_ (Just perId)

findAllExpiredByStatus :: [Storage.CaseStatus] -> Maybe UTCTime -> Maybe UTCTime -> Flow [Storage.Case]
findAllExpiredByStatus statuses maybeFrom maybeTo = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrentTime
  DB.findAll dbTable identity (predicate now)
  where
    predicate now Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        ( [ status `B.in_` (B.val_ <$> statuses),
            validTill B.<=. B.val_ now
          ]
            <> maybe [] (\from -> [createdAt B.>=. B.val_ from]) maybeFrom
            <> maybe [] (\to -> [createdAt B.<=. B.val_ to]) maybeTo
        )

updateValidTillFlow :: Id Storage.Case -> UTCTime -> Flow ()
updateValidTillFlow id validTill = DB.runSqlDB (updateValidTill id validTill)

updateValidTill :: Id Storage.Case -> UTCTime -> DB.SqlDB ()
updateValidTill cid validTill_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause validTill_ currTime)
    (predicate cid)
  where
    setClause scValidTill currTime Storage.Case {..} =
      mconcat
        [ validTill <-. B.val_ scValidTill,
          updatedAt <-. B.val_ currTime
        ]
    predicate cid_ Storage.Case {..} = id ==. B.val_ cid_

updateStatusFlow :: Id Storage.Case -> Storage.CaseStatus -> Flow ()
updateStatusFlow id status = DB.runSqlDB (updateStatus id status)

updateStatus :: Id Storage.Case -> Storage.CaseStatus -> DB.SqlDB ()
updateStatus cid status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate cid)
  where
    setClause pStatus currTime Storage.Case {..} =
      mconcat
        [ status <-. B.val_ pStatus,
          updatedAt <-. B.val_ currTime
        ]
    predicate cid_ Storage.Case {..} = id ==. B.val_ cid_

findAllWithLimitOffsetWhere :: [Id Loc.Location] -> [Id Loc.Location] -> [Storage.CaseType] -> [Storage.CaseStatus] -> [Text] -> Maybe Int -> Maybe Int -> Flow [Storage.Case]
findAllWithLimitOffsetWhere fromLocationIds toLocationIds types statuses udf1s mlimit moffset = do
  dbTable <- getDbTable
  DB.findAll
    dbTable
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    predicate
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ fromLocationId `B.in_` (B.val_ <$> fromLocationIds) ||. complementVal fromLocationIds,
          toLocationId `B.in_` (B.val_ <$> toLocationIds) ||. complementVal toLocationIds,
          status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses,
          _type `B.in_` (B.val_ <$> types) ||. complementVal types,
          udf1 `B.in_` (B.val_ . Just <$> udf1s) ||. complementVal udf1s
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

updateInfoFlow :: Id Storage.Case -> Text -> Flow ()
updateInfoFlow caseId csInfo = do
  DB.runSqlDB (updateInfo caseId csInfo)

updateInfo :: Id Storage.Case -> Text -> DB.SqlDB ()
updateInfo caseId csInfo = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update' dbTable (setClause csInfo currTime) (predicate caseId)
  where
    setClause cInfo currTime' Storage.Case {..} =
      mconcat
        [ info <-. B.val_ (Just cInfo),
          updatedAt <-. B.val_ currTime'
        ]
    predicate pcaseId Storage.Case {..} = id ==. B.val_ pcaseId
