module Storage.Queries.AllocationEvent where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id (Id)
import Beckn.Types.Schema
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.AllocationEvent as Storage
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.AllocationEventT))
getDbTable =
  DB.allocationEvent . DB.transporterDb <$> getSchemaName

create :: Storage.AllocationEvent -> Flow ()
create allocationEvent = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression allocationEvent)

findAllocationEventById ::
  Id Storage.AllocationEvent -> Flow (Maybe Storage.AllocationEvent)
findAllocationEventById aeId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.AllocationEvent {..} = id ==. B.val_ aeId

logAllocationEvent :: Storage.AllocationEventType -> Id Ride -> Maybe (Id Driver) -> Flow ()
logAllocationEvent eventType rideId driverId = do
  uuid <- generateGUID
  now <- getCurrentTime
  create $
    Storage.AllocationEvent
      { id = uuid,
        eventType = eventType,
        timestamp = now,
        driverId = driverId,
        rideId = rideId
      }
