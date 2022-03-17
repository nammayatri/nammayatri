module Storage.Queries.SearchRequest where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Types.Storage.Organization
import qualified Types.Storage.SearchRequest as Storage

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.SearchRequestT))
getDbTable =
  DB.searchRequest . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.SearchRequest -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.SearchRequest -> DB.SqlDB ()
create searchRequest = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue searchRequest)

findAllByIds :: DBFlow m r => [Id Storage.SearchRequest] -> m [Storage.SearchRequest]
findAllByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.SearchRequest {..} =
      B.in_ id (B.val_ <$> ids)

findById :: DBFlow m r => Id Storage.SearchRequest -> m (Maybe Storage.SearchRequest)
findById searchRequestId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchRequest {..} = id ==. B.val_ searchRequestId

findByTxnIdAndBapIdAndBppId :: DBFlow m r => Text -> Text -> Id Organization -> m (Maybe Storage.SearchRequest)
findByTxnIdAndBapIdAndBppId txnId bapId_ orgId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchRequest {..} =
      transactionId ==. B.val_ txnId
        &&. providerId ==. B.val_ orgId
        &&. bapId ==. B.val_ bapId_

findAllNotExpiredByOrgId ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Id Organization ->
  UTCTime ->
  m [Storage.SearchRequest]
findAllNotExpiredByOrgId limit offset orgId now = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.SearchRequest {..} = B.desc_ createdAt
    predicate Storage.SearchRequest {..} =
      providerId ==. B.val_ orgId
        &&. validTill B.>. B.val_ now

findAllByTime ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Id Organization ->
  UTCTime ->
  UTCTime ->
  m [Storage.SearchRequest]
findAllByTime limit offset orgId now fromTime = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.SearchRequest {..} = B.desc_ createdAt
    predicate Storage.SearchRequest {..} =
      providerId ==. B.val_ orgId
        &&. validTill B.>. B.val_ now
        &&. createdAt B.<. B.val_ fromTime

findAllExpired :: DBFlow m r => UTCTime -> UTCTime -> m [Storage.SearchRequest]
findAllExpired from to = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrentTime
  DB.findAll dbTable identity (predicate now)
  where
    predicate now Storage.SearchRequest {..} =
      validTill B.<=. B.val_ now
        &&. createdAt B.>=. B.val_ from
        &&. createdAt B.<=. B.val_ to
