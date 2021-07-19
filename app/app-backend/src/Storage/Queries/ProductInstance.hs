module Storage.Queries.ProductInstance where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.ProductInstance as Storage
import Types.Storage.Products
import qualified Types.Storage.SearchRequest as SearchRequest

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductInstanceT))
getDbTable = DB.productInstance . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.ProductInstance -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.ProductInstance -> DB.SqlDB ()
create productInstance = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue productInstance)

findById :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate piid Storage.ProductInstance {..} = id ==. B.val_ piid
        
findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.ProductInstance]
findAllByRequestId searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      requestId ==. B.val_ searchRequestId

findByProductId :: DBFlow m r => Id Products -> m (Maybe Storage.ProductInstance)
findByProductId pId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      productId ==. B.val_ pId

updateRequestId ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Id SearchRequest.SearchRequest ->
  m ()
updateRequestId piId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate piId)
  where
    predicate piid Storage.ProductInstance {..} = id ==. B.val_ piid
    setClause searchRequestId_ currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ searchRequestId_
        ]

updateStatus ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatus piId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate piId)
  where
    predicate piid Storage.ProductInstance {..} = id ==. B.val_ piid
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateAllProductInstancesByRequestId ::
  DBFlow m r =>
  Id SearchRequest.SearchRequest ->
  Storage.ProductInstanceStatus ->
  m ()
updateAllProductInstancesByRequestId searchRequestId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate searchRequestId)
  where
    predicate searchRequestId_ Storage.ProductInstance {..} = requestId ==. B.val_ searchRequestId_
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

listAllProductInstanceWithOffset ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Storage.ListById ->
  [Storage.ProductInstanceStatus] ->
  m [Storage.ProductInstance]
listAllProductInstanceWithOffset limit offset lbid stats = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy) (predicate lbid stats)
  where
    predicate (Storage.ByApplicationId i) s Storage.ProductInstance {..} =
      requestId ==. B.val_ i
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    predicate (Storage.ByCustomerId i) s Storage.ProductInstance {..} =
      personId ==. B.val_ (Just i)
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    predicate (Storage.ById i) s Storage.ProductInstance {..} =
      productId ==. B.val_ i
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    orderBy Storage.ProductInstance {..} = B.desc_ updatedAt

listAllProductInstance ::
  DBFlow m r =>
  Storage.ListById ->
  [Storage.ProductInstanceStatus] ->
  m [Storage.ProductInstance]
listAllProductInstance piId status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate piId status_)
  where
    predicate (Storage.ByApplicationId i) [] Storage.ProductInstance {..} = requestId ==. B.val_ i
    predicate (Storage.ByApplicationId i) s Storage.ProductInstance {..} = requestId ==. B.val_ i &&. B.in_ status (B.val_ <$> s)
    predicate (Storage.ByCustomerId i) [] Storage.ProductInstance {..} = personId ==. B.val_ (Just i)
    predicate (Storage.ByCustomerId i) s Storage.ProductInstance {..} = personId ==. B.val_ (Just i) &&. B.in_ status (B.val_ <$> s)
    predicate (Storage.ById i) [] Storage.ProductInstance {..} = productId ==. B.val_ i
    predicate (Storage.ById i) s Storage.ProductInstance {..} = productId ==. B.val_ i &&. B.in_ status (B.val_ <$> s)

updateMultipleFlow ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Storage.ProductInstance ->
  m ()
updateMultipleFlow id prdInst = DB.runSqlDB (updateMultiple id prdInst)

updateMultiple :: Id Storage.ProductInstance -> Storage.ProductInstance -> DB.SqlDB ()
updateMultiple piId prdInst = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update' dbTable (setClause currTime prdInst) (predicate piId)
  where
    predicate piid Storage.ProductInstance {..} = id ==. B.val_ piid
    setClause now prodInst Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (Storage.status prodInst),
          --personId <-. B.val_ (Storage.personId prd),
          fromLocation <-. B.val_ (Storage.fromLocation prodInst),
          toLocation <-. B.val_ (Storage.toLocation prodInst),
          info <-. B.val_ (Storage.info prodInst),
          udf4 <-. B.val_ (Storage.udf4 prodInst),
          actualPrice <-. B.val_ (Storage.actualPrice prodInst),
          actualDistance <-. B.val_ (Storage.actualDistance prodInst)
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllExpiredByStatus ::
  DBFlow m r =>
  [Storage.ProductInstanceStatus] ->
  UTCTime ->
  m [Storage.ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime