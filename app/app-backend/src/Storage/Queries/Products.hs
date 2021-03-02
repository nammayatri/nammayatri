module Storage.Queries.Products where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Products as Storage
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductsT))
getDbTable =
  DB._products . DB.appDb <$> getSchemaName

create :: Storage.Products -> Flow (T.DBResult ())
create Storage.Products {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})

findById :: ID Storage.Products -> Flow (T.DBResult (Maybe Storage.Products))
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate pid)
  where
    predicate id Storage.Products {..} = _id ==. B.val_ id

findAllByIds :: [ID Storage.Products] -> Flow (T.DBResult [Storage.Products])
findAllByIds pids = do
  dbTable <- getDbTable
  DB.findAll dbTable (predicate pids)
  where
    predicate ids Storage.Products {..} =
      _id `B.in_` (B.val_ <$> ids)
