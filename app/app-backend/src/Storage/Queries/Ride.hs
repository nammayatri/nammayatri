{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Ride where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
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
import qualified Types.Storage.OldRide as Storage
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Quote as SQuote
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
updateStatus prodInstId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.Ride {..} =
      id ==. B.val_ pId
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
updateRequestId prodInstId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate prodInstId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause scRequestId currTime Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ scRequestId
        ]

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

updateInfo :: Id Storage.Ride -> Text -> DB.SqlDB ()
updateInfo prodInstId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate prodInstId)
  where
    predicate piId Storage.Ride {..} = id ==. B.val_ piId
    setClause pInfo Storage.Ride {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

findAllByPersonId :: DBFlow m r => Id Person -> m [Storage.Ride]
findAllByPersonId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Ride {..} = personId ==. B.val_ (Just piId)

findByQuoteId :: DBFlow m r => Id SQuote.Quote -> m (Maybe Storage.Ride)
findByQuoteId quote = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Ride {..} =
      quoteId ==. B.val_ quote

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

findAllByPerson :: DBFlow m r => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.Ride]
findAllByPerson perId mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset) $ predicate isOnlyActive
  where
    predicate isOnlyActive Storage.Ride {..} =
      personId ==. B.val_ (Just perId)
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True

updateMultipleFlow ::
  DBFlow m r =>
  Id Storage.Ride ->
  Storage.Ride ->
  m ()
updateMultipleFlow id ride = DB.runSqlDB (updateMultiple id ride)

updateMultiple :: Id Storage.Ride -> Storage.Ride -> DB.SqlDB ()
updateMultiple rideId ride = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update' dbTable (setClause currTime ride) (predicate rideId)
  where
    predicate rideId_ Storage.Ride {..} = id ==. B.val_ rideId_
    setClause now ride_ Storage.Ride {..} =
      mconcat
        [ updatedAt <-. B.val_ now,
          status <-. B.val_ (ride_.status),
          --personId <-. B.val_ (Storage.personId prd),
          fromLocation <-. B.val_ (ride_.fromLocation),
          toLocation <-. B.val_ (ride_.toLocation),
          info <-. B.val_ (ride_.info),
          udf4 <-. B.val_ (ride_.udf4),
          actualPrice <-. B.val_ (ride_.actualPrice),
          actualDistance <-. B.val_ (ride_.actualDistance)
        ]


findByRBId :: DBFlow m r => Id SRB.RideBooking -> m (Maybe Storage.Ride)
findByRBId rbId = do
  dbTable <- getDbTable
  list <- DB.findAll dbTable (B.orderBy_ orderBy) predicate
  return $ listToMaybe list
  where
    orderBy Storage.Ride {..} = B.desc_ createdAt
    predicate Storage.Ride {..} = bookingId ==. B.val_ rbId