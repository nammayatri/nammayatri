module Storage.Queries.Customer where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Types.Storage.Customer           as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CustomerT)
dbTable = DB._customer DB.transporterDb

create :: Storage.Customer -> L.Flow ()
create Storage.Customer {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Customer {..}) >>=
  either DB.throwDBError pure

findCustomerById ::
     CustomerId -> L.Flow (Maybe Storage.Customer)
findCustomerById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Customer {..} = (_id ==. B.val_ id)
