module Storage.Queries.Products where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Products as Storage
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.transporterDb

create :: Storage.Products -> Flow ()
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})
    >>= either DB.throwDBError pure

findAllById :: [ProductsId] -> Flow [Storage.Products]
findAllById ids =
  DB.findAllOrErr dbTable (predicate ids)
  where
    predicate ids Storage.Products {..} =
      B.in_ _id (B.val_ <$> ids)

findById :: ProductsId -> Flow Storage.Products
findById pid =
  DB.findOneWithErr dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. B.val_ pid

findById' :: ProductsId -> Flow (T.DBResult (Maybe Storage.Products))
findById' pid =
  DB.findOne dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. B.val_ pid

findByName :: Text -> Flow Storage.Products
findByName name =
  DB.findOneWithErr dbTable (predicate name)
  where
    predicate name Storage.Products {..} =
      _name ==. B.val_ name
