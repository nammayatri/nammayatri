{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import Types.Storage.Person (Person)
import qualified Types.Storage.Products as Product
import qualified Types.Storage.Quote as PI
import qualified Types.Storage.Ride as Storage
import qualified Types.Storage.SearchRequest as SearchRequest
import Types.Storage.Vehicle (Vehicle)
import Beckn.Types.Amount

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RideT))
getDbTable = DB.ride . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Ride -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Ride -> DB.SqlDB ()
create ride = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue ride)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.Ride]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Ride {..} = B.desc_ createdAt
    predicate Storage.Ride {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Ride]
findAllByRequestId reqId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = requestId ==. B.val_ reqId

findByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m (Maybe Storage.Ride)
findByRequestId reqId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = requestId ==. B.val_ reqId

findAllByRequestId' :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Ride]
findAllByRequestId' searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      requestId ==. B.val_ searchRequestId

findAllByIds' :: DBFlow m r => [Id Storage.Ride] -> m [Storage.Ride]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusForProducts :: DBFlow m r => Id Product.Products -> Storage.RideStatus -> m ()
updateStatusForProducts productId_ status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate productId_)
  where
    predicate pId Storage.Ride {..} = productId ==. B.val_ pId
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.Ride ->
  Storage.RideStatus ->
  m ()
updateStatusFlow rideId status = DB.runSqlDB (updateStatus rideId status)

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
    predicate rideId_ Storage.Ride {..} =
      id ==. B.val_ rideId_
    setClause scStatus currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

findAllByRequestIds :: DBFlow m r => [Id SearchRequest.SearchRequest] -> m [Storage.Ride]
findAllByRequestIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ requestId (B.val_ <$> ids)

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

updateRequestId ::
  DBFlow m r =>
  Id Storage.Ride ->
  Id SearchRequest.SearchRequest ->
  m ()
updateRequestId rideId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause scRequestId currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ scRequestId
        ]

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.Ride]
findAllByProdId prodId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = productId ==. B.val_ prodId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findById :: DBFlow m r => Id Storage.Ride -> m (Maybe Storage.Ride)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} = id ==. B.val_ pid

updateDriverFlow :: DBFlow m r => Id Storage.Ride -> Maybe (Id Person) -> m ()
updateDriverFlow rideId driverId =
  DB.runSqlDB (updateDriver rideId driverId)

updateDriver ::
  Id Storage.Ride ->
  Maybe (Id Person) ->
  DB.SqlDB ()
updateDriver rideId driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause driverId now)
    (predicate rideId)
  where
    predicate rideId' Storage.Ride {..} = id B.==. B.val_ rideId'
    setClause sDriverId currTime Storage.Ride {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateVehicleFlow :: DBFlow m r => Id Storage.Ride -> Maybe Text -> m ()
updateVehicleFlow rideId vehId = do
  DB.runSqlDB (updateVehicle rideId (Id <$> vehId))

updateVehicle ::
  Id Storage.Ride ->
  Maybe (Id Vehicle) ->
  DB.SqlDB ()
updateVehicle rideId vehId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause (getId <$> vehId) now)
    (predicate rideId)
  where
    predicate rideId' Storage.Ride {..} = id B.==. B.val_ rideId'
    setClause vehicleId currTime' Storage.Ride {..} =
      mconcat
        [ entityId <-. B.val_ vehicleId,
          updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.Ride -> Text -> DB.SqlDB ()
updateInfo rideId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause pInfo Storage.Ride {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

findAllByVehicleId :: DBFlow m r => Maybe (Id Vehicle) -> m [Storage.Ride]
findAllByVehicleId vehId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = B.val_ (isJust vehId) &&. entityId ==. B.val_ (getId <$> vehId)

findAllByPersonId :: DBFlow m r => Id Person -> m [Storage.Ride]
findAllByPersonId personId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = personId ==. B.val_ (Just personId_)

findByQuoteId :: DBFlow m r => Id PI.Quote -> m (Maybe Storage.Ride)
findByQuoteId quoteId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} =
      quoteId ==. B.val_ quoteId_

findAllExpiredByStatus :: DBFlow m r => [Storage.RideStatus] -> UTCTime -> m [Storage.Ride]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

getCountByStatus :: DBFlow m r => Id Org.Organization -> m [(Storage.RideStatus, Int)]
getCountByStatus orgId = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.aggregate_ aggregator) predicate
  where
    aggregator Storage.Ride {..} = (B.group_ status, B.as_ @Int B.countAll_)
    predicate Storage.Ride {..} =
      organizationId ==. B.val_ orgId

findByStartTimeBuffer ::
  DBFlow m r =>
  UTCTime ->
  NominalDiffTime ->
  [Storage.RideStatus] ->
  m [Storage.Ride]
findByStartTimeBuffer startTime_ buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime_
  let toTime = addUTCTime (buffer * 60 * 60) startTime_
  DB.findAll dbTable identity (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.Ride {..} =
      let inStatus = fmap B.val_ statuses
       in startTime B.<=. B.val_ toTime
            &&. startTime B.>=. B.val_ fromTime
            &&. status `B.in_` inStatus

getDriverCompletedRides :: DBFlow m r => Id Person -> UTCTime -> UTCTime -> m [Storage.Ride]
getDriverCompletedRides driverId fromTime toTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} =
      personId ==. B.val_ (Just driverId)
        &&. status ==. B.val_ Storage.COMPLETED
        &&. startTime B.>=. B.val_ fromTime
        &&. startTime B.<=. B.val_ toTime

updateActualPrice :: Amount -> Id Storage.Ride -> DB.SqlDB ()
updateActualPrice price' rideId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate rideId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.Ride {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

updateDistance ::
  Id Person ->
  Double ->
  DB.SqlDB ()
updateDistance driverId distance' = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause distance')
    (predicate driverId)
  where
    predicate driverId' Storage.Ride {..} =
      personId ==. B.val_ (Just driverId')
        &&. status ==. B.val_ Storage.INPROGRESS
    setClause distance'' Storage.Ride {..} = distance <-. B.current_ distance + B.val_ distance''

findAllByOrg :: DBFlow m r => Id Org.Organization -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.Ride]
findAllByOrg orgId mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset) $ predicate isOnlyActive
  where
    predicate isOnlyActive Storage.Ride {..} =
      organizationId ==. B.val_ orgId
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True

findAllByDriver :: DBFlow m r => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.Ride]
findAllByDriver driverId mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset) $ predicate isOnlyActive
  where
    predicate isOnlyActive Storage.Ride {..} =
      personId ==. B.val_ (Just driverId)
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True
