module Storage.Queries.Customer where

import App.Types
import Beckn.External.Encryption
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Customer as Storage
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CustomerT))
getDbTable =
  DB._customer . DB.transporterDb <$> getSchemaName

create :: Storage.Customer -> Flow ()
create customer = do
  dbTable <- getDbTable
  customer' <- encrypt customer
  DB.createOne dbTable (Storage.insertExpression customer')
    >>= either throwDBError pure

findCustomerById ::
  Id Storage.Customer -> Flow (Maybe Storage.Customer)
findCustomerById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either throwDBError pure
    >>= decrypt
  where
    predicate Storage.Customer {..} = _id ==. B.val_ id
