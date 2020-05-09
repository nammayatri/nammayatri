module Storage.Queries.Tracker where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Types.Storage.Tracker             as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TrackerT)
dbTable = DB._tracker DB.transporterDb

create :: Storage.Tracker -> L.Flow ()
create Storage.Tracker {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Tracker {..}) >>=
  either DB.throwDBError pure

findTrackerById ::
     TrackerId -> L.Flow (Maybe Storage.Tracker)
findTrackerById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Tracker {..} = (_id ==. B.val_ id)
