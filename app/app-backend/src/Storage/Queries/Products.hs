module Storage.Queries.Products where

import App.Types
import Beckn.Types.App
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

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.ProductsT)
dbTable = DB._products DB.appDb

create :: Storage.Products -> Flow (T.DBResult ())
create Storage.Products {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Products {..})

findById :: ProductsId -> Flow (T.DBResult (Maybe Storage.Products))
findById pid =
  DB.findOne dbTable (predicate pid)
  where
    predicate pid Storage.Products {..} = _id ==. B.val_ pid

findAllByIds :: [ProductsId] -> Flow (T.DBResult [Storage.Products])
findAllByIds pids =
  DB.findAll dbTable (predicate pids)
  where
    predicate pids Storage.Products {..} =
      _id `B.in_` (B.val_ <$> pids)
