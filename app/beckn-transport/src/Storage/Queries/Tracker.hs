module Storage.Queries.Tracker where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Tracker as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TrackerT))
getDbTable =
  DB._tracker . DB.transporterDb <$> getSchemaName

create :: Storage.Tracker -> Flow ()
create Storage.Tracker {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Tracker {..})

findTrackerById ::
  Id Storage.Tracker -> Flow (Maybe Storage.Tracker)
findTrackerById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Tracker {..} = _id ==. B.val_ id
