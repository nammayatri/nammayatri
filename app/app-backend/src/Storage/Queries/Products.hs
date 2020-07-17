module Storage.Queries.Products where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Products as Storage
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries as DB
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.appDb

create :: Storage.Products -> Flow ()
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})
    >>= either DB.throwDBError pure

findById :: ProductsId -> Flow Storage.Products
findById pid =
  DB.findOneWithErr dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. B.val_ pid

findAllByIds :: [ProductsId] -> Flow [Storage.Products]
findAllByIds pids =
  DB.findAll dbTable (predicate pids)
    >>= either DB.throwDBError pure
  where
    predicate pids Storage.Products {..} =
      _id `B.in_` (B.val_ <$> pids)
