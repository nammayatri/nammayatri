module Storage.Queries.Products where

import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Products as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
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

findByName :: Text -> Flow Storage.Products
findByName name =
  DB.findOneWithErr dbTable (predicate name)
  where
    predicate name Storage.Products {..} =
      _name ==. B.val_ name
