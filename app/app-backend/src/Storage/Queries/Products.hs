module Storage.Queries.Products where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Products as Storage
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductsT))
getDbTable =
  DB.products . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Products -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Products -> DB.SqlDB ()
create Storage.Products {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression Storage.Products {..})

findById :: DBFlow m r => Id Storage.Products -> m (Maybe Storage.Products)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate pid_ Storage.Products {..} = id ==. B.val_ pid_

findAllByIds :: DBFlow m r => [Id Storage.Products] -> m [Storage.Products]
findAllByIds pids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate pids)
  where
    predicate ids Storage.Products {..} =
      id `B.in_` (B.val_ <$> ids)
