module Storage.Queries.AllocationEvent where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common (generateGUID)
import Beckn.Types.ID (ID)
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (RideId)
import qualified Types.Storage.AllocationEvent as Storage
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.AllocationEventT))
getDbTable =
  DB._allocationEvent . DB.transporterDb <$> getSchemaName

create :: Storage.AllocationEvent -> Flow ()
create allocationEvent = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression allocationEvent)
    >>= either DB.throwDBError pure

findAllocationEventById ::
  ID Storage.AllocationEvent -> Flow (Maybe Storage.AllocationEvent)
findAllocationEventById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.AllocationEvent {..} = _id ==. B.val_ id

logAllocationEvent :: Storage.AllocationEventType -> RideId -> Flow ()
logAllocationEvent eventType rideId = do
  uuid <- generateGUID
  now <- getCurrTime
  create $
    Storage.AllocationEvent
      { _id = uuid,
        _eventType = eventType,
        _timestamp = now,
        _rideId = rideId
      }
