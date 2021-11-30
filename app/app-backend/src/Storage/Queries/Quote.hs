module Storage.Queries.Quote where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Quote as Storage
import qualified Types.Storage.SearchRequest as SearchRequest

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

findByBPPQuoteId :: DBFlow m r => Id Storage.BPPQuote -> m (Maybe Storage.Quote)
findByBPPQuoteId bppQuoteId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} = bppQuoteId ==. B.val_ bppQuoteId_

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      requestId ==. B.val_ searchRequestId