module Storage.Queries.Driver where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Driver as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverT)
dbTable = DB._driver DB.transporterDb

create :: Storage.Driver -> L.Flow ()
create Storage.Driver {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Driver {..})
    >>= either DB.throwDBError pure

findDriverById ::
  DriverId -> L.Flow (Maybe Storage.Driver)
findDriverById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Driver {..} = (_id ==. B.val_ id)
