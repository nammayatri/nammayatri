module Storage.Queries.Case where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Case as Storage
import qualified Types.Storage.DB as DB
import Types.Storage.Organization

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Case -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.Case -> DB.SqlDB ()
create case_ = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue case_)

findAllByIds :: DBFlow m r => [Id Storage.Case] -> m [Storage.Case]
findAllByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} =
      B.in_ id (B.val_ <$> ids)

findById :: DBFlow m r => Id Storage.Case -> m (Maybe Storage.Case)
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = id ==. B.val_ caseId

findBySid :: DBFlow m r => Text -> m (Maybe Storage.Case)
findBySid sid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = shortId ==. B.val_ (ShortId sid)

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.Case ->
  Storage.CaseStatus ->
  m ()
updateStatusFlow id newStatus = DB.runSqlDB (updateStatus id newStatus)

updateStatus ::
  Id Storage.Case ->
  Storage.CaseStatus ->
  DB.SqlDB ()
updateStatus caseId newStatus = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause newStatus currTime)
    (predicate caseId)
  where
    predicate cid Storage.Case {..} = id ==. B.val_ cid
    setClause status_ currTime Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ status_
        ]

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.Case] ->
  Storage.CaseStatus ->
  m ()
updateStatusByIdsFlow ids newStatus = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause newStatus currTime)
    (predicate ids)
  where
    predicate cids Storage.Case {..} = B.in_ id (B.val_ <$> cids)
    setClause status_ currTime Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ status_
        ]

findAllByStatuses ::
  DBFlow m r =>
  Integer ->
  Integer ->
  [Storage.CaseStatus] ->
  Id Organization ->
  UTCTime ->
  m [Storage.Case]
findAllByStatuses limit offset statuses orgId now = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      provider ==. B.val_ (Just $ getId orgId)
        &&. B.in_ status (B.val_ <$> statuses)
        &&. validTill B.>. B.val_ now

findAllByStatusTime ::
  DBFlow m r =>
  Integer ->
  Integer ->
  [Storage.CaseStatus] ->
  Id Organization ->
  UTCTime ->
  UTCTime ->
  m [Storage.Case]
findAllByStatusTime limit offset statuses orgId now fromTime = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      provider ==. B.val_ (Just $ getId orgId)
        &&. B.in_ status (B.val_ <$> statuses)
        &&. validTill B.>. B.val_ now
        &&. createdAt B.<. B.val_ fromTime

findAllExpiredByStatus :: DBFlow m r => [Storage.CaseStatus] -> UTCTime -> UTCTime -> m [Storage.Case]
findAllExpiredByStatus statuses from to = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrentTime
  DB.findAll dbTable identity (predicate now)
  where
    predicate now Storage.Case {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. validTill B.<=. B.val_ now
        &&. createdAt B.>=. B.val_ from
        &&. createdAt B.<=. B.val_ to
