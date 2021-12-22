module Storage.Queries.Ride where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common (identity)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as Pers
import qualified Types.Storage.Ride as Storage
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.Vehicle as Veh

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RideT))
getDbTable = DB.ride . DB.transporterDb <$> getSchemaName

getRBTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SRB.RideBookingT))
getRBTable = DB.rideBooking . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Ride -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Ride -> DB.SqlDB ()
create ride = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue ride)

findById :: DBFlow m r => Id Storage.Ride -> m (Maybe Storage.Ride)
findById rideId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = id ==. B.val_ rideId

findByRBId :: DBFlow m r => Id SRB.RideBooking -> m (Maybe Storage.Ride)
findByRBId rbId = do
  dbTable <- getDbTable
  list <- DB.findAll dbTable (B.orderBy_ orderBy) predicate
  return $ listToMaybe list
  where
    orderBy Storage.Ride {..} = B.desc_ createdAt
    predicate Storage.Ride {..} = bookingId ==. B.val_ rbId

findAllByVehicleId :: DBFlow m r => Id Veh.Vehicle -> m [Storage.Ride]
findAllByVehicleId vehId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = vehicleId ==. B.val_ vehId

findAllByDriverId :: DBFlow m r => Id Pers.Person -> m [Storage.Ride]
findAllByDriverId driverId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = driverId ==. B.val_ driverId_

getInProgressByDriverId :: DBFlow m r => Id Pers.Person -> m (Maybe Storage.Ride)
getInProgressByDriverId driverId' = do
  dbTable <- getDbTable
  DB.findOne dbTable $ \Storage.Ride {..} ->
    driverId ==. B.val_ driverId'
      &&. status ==. B.val_ Storage.INPROGRESS

updateStatus ::
  Id Storage.Ride ->
  Storage.RideStatus ->
  DB.SqlDB ()
updateStatus rideId status_ = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} =
      id ==. B.val_ rideId_
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusByIds ::
  [Id Storage.Ride] ->
  Storage.RideStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.Ride {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateDistance ::
  Id Pers.Person ->
  Double ->
  DB.SqlDB ()
updateDistance driverId_ distance' = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause distance')
    (predicate driverId_)
  where
    predicate driverId' Storage.Ride {..} =
      driverId ==. B.val_ driverId'
        &&. status ==. B.val_ Storage.INPROGRESS
    setClause distance'' Storage.Ride {..} = traveledDistance <-. B.current_ traveledDistance + B.val_ distance''

updateAll ::
  Id Storage.Ride ->
  Storage.Ride ->
  DB.SqlDB ()
updateAll rideId ride = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause now ride)
    (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause currTime ride_ Storage.Ride {..} =
      mconcat
        [ status <-. B.val_ ride_.status,
          fare <-. B.val_ ride_.fare,
          totalFare <-. B.val_ ride_.totalFare,
          chargeableDistance <-. B.val_ ride_.chargeableDistance,
          updatedAt <-. B.val_ currTime
        ]

getCountByStatus :: DBFlow m r => Id Org.Organization -> m [(Storage.RideStatus, Int)]
getCountByStatus orgId = do
  dbTable <- getDbTable
  rbTable <- getRBTable
  DB.findAllByJoin (B.aggregate_ aggregator) (query dbTable rbTable)
  where
    aggregator Storage.Ride {..} = (B.group_ status, B.as_ @Int B.countAll_)
    query dbTable rbTable = do
      ride <- B.all_ dbTable
      rideBooking <- B.join_ rbTable $ \row ->
        row.id B.==. ride.bookingId
      B.guard_ $ rideBooking.providerId ==. B.val_ orgId
      return ride
