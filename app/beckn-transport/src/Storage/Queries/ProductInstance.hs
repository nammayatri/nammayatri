module Storage.Queries.ProductInstance where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.SearchReqLocation as Loc
import qualified Types.Storage.DB as DB
import Types.Storage.Person (Person)
import qualified Types.Storage.ProductInstance as Storage
import qualified Types.Storage.Products as Product
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchRequest as SearchRequest
import Types.Storage.Vehicle (Vehicle)

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.ProductInstanceT))
getDbTable = DB.productInstance . DB.transporterDb <$> getSchemaName

getSRTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SearchRequest.SearchRequestT))
getSRTable =
  DB.searchRequest . DB.transporterDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.transporterDb <$> getSchemaName

getRideTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Ride.RideT))
getRideTable =
  DB.ride . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.ProductInstance -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.ProductInstance -> DB.SqlDB ()
create productInstance = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue productInstance)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.ProductInstance]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.ProductInstance {..} = B.desc_ createdAt
    predicate Storage.ProductInstance {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.ProductInstance]
findAllByRequestId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = requestId ==. B.val_ piId

findByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m (Maybe Storage.ProductInstance)
findByRequestId piId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} = requestId ==. B.val_ piId

findById' :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById' productInstanceId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} =
      id ==. B.val_ productInstanceId

findAllByRequestId' :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.ProductInstance]
findAllByRequestId' searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      requestId ==. B.val_ searchRequestId

findAllByIds' :: DBFlow m r => [Id Storage.ProductInstance] -> m [Storage.ProductInstance]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusForProducts :: DBFlow m r => Id Product.Products -> Storage.ProductInstanceStatus -> m ()
updateStatusForProducts productId_ status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate productId_)
  where
    predicate pId Storage.ProductInstance {..} = productId ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusFlow prodInstId status = DB.runSqlDB (updateStatus prodInstId status)

updateStatus ::
  Id Storage.ProductInstance ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatus prodInstId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate prodInstId)
  where
    predicate pId Storage.ProductInstance {..} =
      id ==. B.val_ pId
    setClause scStatus currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

findAllByRequestIds :: DBFlow m r => [Id SearchRequest.SearchRequest] -> m [Storage.ProductInstance]
findAllByRequestIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ requestId (B.val_ <$> ids)

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.ProductInstance] ->
  Storage.ProductInstanceStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateRequestId ::
  DBFlow m r =>
  Id Storage.ProductInstance ->
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
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause scRequestId currTime Storage.ProductInstance {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ scRequestId
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
    predicate driverId' Storage.ProductInstance {..} =
      personId ==. B.val_ (Just driverId')
        &&. status ==. B.val_ Storage.INPROGRESS
    setClause distance'' Storage.ProductInstance {..} = distance <-. B.current_ distance + B.val_ distance''

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.ProductInstance]
findAllByProdId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = productId ==. B.val_ piId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findById :: DBFlow m r => Id Storage.ProductInstance -> m (Maybe Storage.ProductInstance)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.ProductInstance {..} = id ==. B.val_ pid

updateDriverFlow :: DBFlow m r => [Id Storage.ProductInstance] -> Maybe (Id Person) -> m ()
updateDriverFlow ids driverId =
  DB.runSqlDB (updateDriver ids driverId)

updateDriver ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Person) ->
  DB.SqlDB ()
updateDriver ids driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause driverId now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.ProductInstance {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateVehicleFlow :: DBFlow m r => [Id Storage.ProductInstance] -> Maybe Text -> m ()
updateVehicleFlow ids vehId = do
  DB.runSqlDB (updateVehicle ids (Id <$> vehId))

updateVehicle ::
  [Id Storage.ProductInstance] ->
  Maybe (Id Vehicle) ->
  DB.SqlDB ()
updateVehicle ids vehId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause (getId <$> vehId) now)
    (predicate ids)
  where
    predicate pids Storage.ProductInstance {..} = id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime' Storage.ProductInstance {..} =
      mconcat
        [ entityId <-. B.val_ vehicleId,
          updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.ProductInstance -> Text -> DB.SqlDB ()
updateInfo prodInstId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause pInfo Storage.ProductInstance {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

updateActualPrice :: Amount -> Id Storage.ProductInstance -> DB.SqlDB ()
updateActualPrice price' prodInstId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate prodInstId)
  where
    predicate piId Storage.ProductInstance {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.ProductInstance {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

findAllByVehicleId :: DBFlow m r => Maybe (Id Vehicle) -> m [Storage.ProductInstance]
findAllByVehicleId piId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} = B.val_ (isJust piId) &&. entityId ==. B.val_ (getId <$> piId)

findAllRidesWithLocationsByDriverId ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Id Person ->
  m [(Storage.ProductInstance, Loc.SearchReqLocation, Loc.SearchReqLocation)]
findAllRidesWithLocationsByDriverId limit offset personId_ = do
  piTable <- getDbTable
  locTable <- Loc.getDbTable
  DB.findAllByJoin
    (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc)
    (joinQuery piTable locTable)
  where
    joinQuery piTable locTable = do
      ride <- B.filter_ predicate $ B.all_ piTable
      fromLoc <- B.join_ locTable $ \loc -> B.just_ loc.id ==. ride.fromLocation
      toLoc <- B.join_ locTable $ \loc -> B.just_ loc.id ==. ride.toLocation
      return (ride, fromLoc, toLoc)
    predicate Storage.ProductInstance {..} =
      personId ==. B.val_ (Just personId_)
    orderByDesc (Storage.ProductInstance {..}, _, _) = B.desc_ createdAt

findAllExpiredByStatus :: DBFlow m r => [Storage.ProductInstanceStatus] -> UTCTime -> m [Storage.ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.ProductInstance {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

findByStartTimeBuffer ::
  DBFlow m r =>
  UTCTime ->
  NominalDiffTime ->
  [Storage.ProductInstanceStatus] ->
  m [Storage.ProductInstance]
findByStartTimeBuffer startTime_ buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime_
  let toTime = addUTCTime (buffer * 60 * 60) startTime_
  DB.findAll dbTable identity (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.ProductInstance {..} =
      let inStatus = fmap B.val_ statuses
       in startTime B.<=. B.val_ toTime
            &&. startTime B.>=. B.val_ fromTime
            &&. status `B.in_` inStatus
