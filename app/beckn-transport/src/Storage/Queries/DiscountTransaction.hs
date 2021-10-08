module Storage.Queries.DiscountTransaction where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DiscountTransaction as Storage

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DiscountTransactionT))
getDbTable =
  DB.discountTransaction . DB.transporterDb <$> getSchemaName

create :: Storage.DiscountTransaction -> DB.SqlDB ()
create Storage.DiscountTransaction {..} = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue Storage.DiscountTransaction {..})
