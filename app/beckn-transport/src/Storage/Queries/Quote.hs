module Storage.Queries.Quote where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Products as Product
import qualified Types.Storage.Quote as Storage
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchRequest as SearchRequest

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.QuoteT))
getDbTable = DB.quote . DB.transporterDb <$> getSchemaName

getSRTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SearchRequest.SearchRequestT))
getSRTable =
  DB.searchRequest . DB.transporterDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.transporterDb <$> getSchemaName

getRideTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Ride.RideT))
getRideTable =
  DB.ride . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Quote -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Quote -> DB.SqlDB ()
create quote = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue quote)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.Quote]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Quote {..} = B.desc_ createdAt
    predicate Storage.Quote {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = requestId ==. B.val_ quoteId

findByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m (Maybe Storage.Quote)
findByRequestId quoteId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} = requestId ==. B.val_ quoteId

findById' :: DBFlow m r => Id Storage.Quote -> m (Maybe Storage.Quote)
findById' quoteId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} =
      id ==. B.val_ quoteId

findAllByRequestId' :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId' searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      requestId ==. B.val_ searchRequestId

findAllByIds' :: DBFlow m r => [Id Storage.Quote] -> m [Storage.Quote]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ id (B.val_ <$> ids)

findAllByRequestIds :: DBFlow m r => [Id SearchRequest.SearchRequest] -> m [Storage.Quote]
findAllByRequestIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ requestId (B.val_ <$> ids)

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.Quote]
findAllByProdId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = productId ==. B.val_ quoteId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findById :: DBFlow m r => Id Storage.Quote -> m (Maybe Storage.Quote)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} = id ==. B.val_ pid
