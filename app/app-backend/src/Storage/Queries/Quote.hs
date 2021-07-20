module Storage.Queries.Quote where

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
import qualified Types.Storage.Person as Person
import Types.Storage.Products
import qualified Types.Storage.Quote as Storage
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Organization as Org

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.QuoteT))
getDbTable = DB.quote . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Quote -> m ()
createFlow = DB.runSqlDB . create

create :: Storage.Quote -> DB.SqlDB ()
create quote = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue quote)

findById :: DBFlow m r => Id Storage.Quote -> m (Maybe Storage.Quote)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate piid Storage.Quote {..} = id ==. B.val_ piid

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      requestId ==. B.val_ searchRequestId

findByProductId :: DBFlow m r => Id Products -> m (Maybe Storage.Quote)
findByProductId pId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} =
      productId ==. B.val_ pId

findAllByPerson :: DBFlow m r => Id Person.Person -> m [Storage.Quote]
findAllByPerson perId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = personId ==. B.val_ (Just perId)

updateRequestId ::
  DBFlow m r =>
  Id Storage.Quote ->
  Id SearchRequest.SearchRequest ->
  m ()
updateRequestId quoteId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate quoteId)
  where
    predicate piid Storage.Quote {..} = id ==. B.val_ piid
    setClause searchRequestId_ currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ searchRequestId_
        ]

updateStatus ::
  DBFlow m r =>
  Id Storage.Quote ->
  Storage.QuoteStatus ->
  m ()
updateStatus quoteId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate quoteId)
  where
    predicate piid Storage.Quote {..} = id ==. B.val_ piid
    setClause scStatus currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateAllQuotesByRequestId ::
  DBFlow m r =>
  Id SearchRequest.SearchRequest ->
  Storage.QuoteStatus ->
  m ()
updateAllQuotesByRequestId searchRequestId status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate searchRequestId)
  where
    predicate searchRequestId_ Storage.Quote {..} = requestId ==. B.val_ searchRequestId_
    setClause scStatus currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

listAllQuoteWithOffset ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Storage.ListById ->
  [Storage.QuoteStatus] ->
  m [Storage.Quote]
listAllQuoteWithOffset limit offset lbid stats = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy) (predicate lbid stats)
  where
    predicate (Storage.ByApplicationId i) s Storage.Quote {..} =
      requestId ==. B.val_ i
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    predicate (Storage.ByCustomerId i) s Storage.Quote {..} =
      personId ==. B.val_ (Just i)
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    predicate (Storage.ById i) s Storage.Quote {..} =
      productId ==. B.val_ i
        &&. (status `B.in_` (B.val_ <$> s) ||. complementVal s)
    orderBy Storage.Quote {..} = B.desc_ updatedAt

listAllQuote ::
  DBFlow m r =>
  Storage.ListById ->
  [Storage.QuoteStatus] ->
  m [Storage.Quote]
listAllQuote quoteId status_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate quoteId status_)
  where
    predicate (Storage.ByApplicationId i) [] Storage.Quote {..} = requestId ==. B.val_ i
    predicate (Storage.ByApplicationId i) s Storage.Quote {..} = requestId ==. B.val_ i &&. B.in_ status (B.val_ <$> s)
    predicate (Storage.ByCustomerId i) [] Storage.Quote {..} = personId ==. B.val_ (Just i)
    predicate (Storage.ByCustomerId i) s Storage.Quote {..} = personId ==. B.val_ (Just i) &&. B.in_ status (B.val_ <$> s)
    predicate (Storage.ById i) [] Storage.Quote {..} = productId ==. B.val_ i
    predicate (Storage.ById i) s Storage.Quote {..} = productId ==. B.val_ i &&. B.in_ status (B.val_ <$> s)

updateMultipleFlow ::
  DBFlow m r =>
  Id Storage.Quote ->
  Storage.Quote ->
  m ()
updateMultipleFlow id prdInst = DB.runSqlDB (updateMultiple id prdInst)

updateMultiple :: Id Storage.Quote -> Storage.Quote -> DB.SqlDB ()
updateMultiple quoteId prdInst = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update' dbTable (setClause currTime prdInst) (predicate quoteId)
  where
    predicate piid Storage.Quote {..} = id ==. B.val_ piid
    setClause now quote Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (Storage.status quote),
          --personId <-. B.val_ (Storage.personId prd),
          fromLocation <-. B.val_ (Storage.fromLocation quote),
          toLocation <-. B.val_ (Storage.toLocation quote),
          info <-. B.val_ (Storage.info quote),
          udf4 <-. B.val_ (Storage.udf4 quote)
        ]

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findAllExpiredByStatus ::
  DBFlow m r =>
  [Storage.QuoteStatus] ->
  UTCTime ->
  m [Storage.Quote]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

countCompletedRides :: DBFlow m r => Id Org.Organization -> m Int
countCompletedRides orgId = do
  dbTable <- getDbTable
  count <- DB.findAll dbTable (B.aggregate_ aggregator) predicate
  return $ case count of
    [cnt] -> cnt
    _ -> 0
  where
    aggregator Storage.Quote {..} = B.countAll_
    predicate Storage.Quote {..} =
      organizationId ==. B.val_ orgId
        &&. status ==. B.val_ Storage.COMPLETED
