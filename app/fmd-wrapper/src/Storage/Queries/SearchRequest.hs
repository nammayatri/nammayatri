module Storage.Queries.SearchRequest where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.SearchRequest as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.SearchRequestT))
getDbTable =
  DB.searchRequest . DB.appDb <$> getSchemaName

create :: DBFlow m r => Storage.SearchRequest -> m ()
create Storage.SearchRequest {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertValue Storage.SearchRequest {..})

findById :: DBFlow m r => Id Storage.SearchRequest -> m (Maybe Storage.SearchRequest)
findById searchRequestId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.SearchRequest {..} = id ==. B.val_ searchRequestId

update :: DBFlow m r => Id Storage.SearchRequest -> Storage.SearchRequest -> m ()
update searchRequestId searchRequest = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update dbTable (setClause currTime searchRequest) (predicate searchRequestId)
  where
    predicate searchRequestId_ Storage.SearchRequest {..} = id ==. B.val_ searchRequestId_
    setClause now searchRequest_ Storage.SearchRequest {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (Storage.status searchRequest_),
          shortId <-. B.val_ (Storage.shortId searchRequest_),
          startTime <-. B.val_ (Storage.startTime searchRequest_),
          endTime <-. B.val_ (Storage.endTime searchRequest_),
          udf1 <-. B.val_ (Storage.udf1 searchRequest_),
          udf2 <-. B.val_ (Storage.udf2 searchRequest_),
          udf3 <-. B.val_ (Storage.udf3 searchRequest_),
          udf4 <-. B.val_ (Storage.udf4 searchRequest_),
          udf5 <-. B.val_ (Storage.udf5 searchRequest_),
          info <-. B.val_ (Storage.info searchRequest_)
        ]
