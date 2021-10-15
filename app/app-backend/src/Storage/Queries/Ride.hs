module Storage.Queries.Ride where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Ride as Storage
import qualified Types.Storage.RideBooking as SRB

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RideT))
getDbTable = DB.ride . DB.appDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Ride -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Ride -> DB.SqlDB ()
create ride = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue ride)

findAllByIds' :: DBFlow m r => [Id Storage.Ride] -> m [Storage.Ride]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.Ride ->
  Storage.RideStatus ->
  m ()
updateStatusFlow prodInstId status = DB.runSqlDB (updateStatus prodInstId status)

updateStatus ::
  Id Storage.Ride ->
  Storage.RideStatus ->
  DB.SqlDB ()
updateStatus rideId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate rideId)
  where
    predicate rId Storage.Ride {..} =
      id ==. B.val_ rId
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.Ride] ->
  Storage.RideStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

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

findById :: DBFlow m r => Id Storage.Ride -> m (Maybe Storage.Ride)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = id ==. B.val_ pid

updateMultiple :: Id Storage.Ride -> Storage.Ride -> DB.SqlDB ()
updateMultiple rideId ride = do
  dbTable <- getDbTable
  currTime <- getCurrentTime
  DB.update' dbTable (setClause currTime ride) (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause now ride_ Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (ride_.status),
          finalPrice <-. B.val_ (ride_.finalPrice),
          totalFare <-. B.val_ (ride_.totalFare),
          chargeableDistance <-. B.val_ (ride_.chargeableDistance)
        ]

findByRBId :: DBFlow m r => Id SRB.RideBooking -> m (Maybe Storage.Ride)
findByRBId rbId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = bookingId ==. B.val_ rbId