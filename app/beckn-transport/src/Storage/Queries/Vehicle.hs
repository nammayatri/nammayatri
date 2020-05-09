module Storage.Queries.Vehicle where

import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified Epass.Storage.Queries as DB
import Epass.Types.Common
import Epass.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Vehicle as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.VehicleT)
dbTable = DB._vehicle DB.transporterDb

create :: Storage.Vehicle -> L.Flow ()
create Storage.Vehicle {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Vehicle {..})
    >>= either DB.throwDBError pure

findVehicleById ::
  VehicleId -> L.Flow (Maybe Storage.Vehicle)
findVehicleById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Vehicle {..} = (_id ==. B.val_ id)
