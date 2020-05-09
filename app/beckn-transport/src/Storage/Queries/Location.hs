module Storage.Queries.Location where

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
import qualified Types.Storage.Location as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.transporterDb

create :: Storage.Location -> L.Flow ()
create Storage.Location {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Location {..})
    >>= either DB.throwDBError pure

findLocationById ::
  LocationId -> L.Flow (Maybe Storage.Location)
findLocationById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)
