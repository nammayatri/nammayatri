module Storage.Queries.OnSearchEvent where

import qualified Beckn.Storage.Common as Storage
import Beckn.Storage.DB.Config
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import Types.Storage.OnSearchEvent

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity OnSearchEventT))
getDbTable = DB.onSearchEvent . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => OnSearchEvent -> m ()
createFlow = DB.runSqlDB . create

create :: OnSearchEvent -> DB.SqlDB ()
create quote = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue quote)
