module Storage.Queries.Customer where

import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.Customer as Storage
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CustomerT)
dbTable = DB._customer DB.transporterDb

create :: Storage.Customer -> L.Flow ()
create Storage.Customer {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Customer {..})
    >>= either DB.throwDBError pure

findCustomerById ::
  CustomerId -> L.Flow (Maybe Storage.Customer)
findCustomerById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Customer {..} = (_id ==. B.val_ id)
