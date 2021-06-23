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
import Types.Error
import qualified Types.Storage.DB as DB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductsT))
getDbTable =
  DB.products . DB.transporterDb <$> getSchemaName

create :: HasFlowDBEnv m r => Storage.Products -> m ()
create Storage.Products {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})

findAllById :: HasFlowDBEnv m r => [Id Storage.Products] -> m [Storage.Products]
findAllById ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate ids)
  where
    predicate pids Storage.Products {..} =
      B.in_ id (B.val_ <$> pids)

findById :: HasFlowDBEnv m r => Id Storage.Products -> m Storage.Products
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate >>= fromMaybeM ProductsNotFound
  where
    predicate Storage.Products {..} = id ==. B.val_ pid

findById' :: HasFlowDBEnv m r => Id Storage.Products -> m (Maybe Storage.Products)
findById' pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Products {..} = id ==. B.val_ pid

findByName :: HasFlowDBEnv m r => Text -> m Storage.Products
findByName name_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate >>= fromMaybeM ProductsNotFound
  where
    predicate Storage.Products {..} =
      name ==. B.val_ name_
