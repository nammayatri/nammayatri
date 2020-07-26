module Storage.Queries.Customer where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.Customer as Storage
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CustomerT)
dbTable = DB._customer DB.transporterDb

create :: Storage.Customer -> Flow ()
create Storage.Customer {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Customer {..})
    >>= either DB.throwDBError pure

findCustomerById ::
  CustomerId -> Flow (Maybe Storage.Customer)
findCustomerById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Customer {..} = _id ==. B.val_ id
