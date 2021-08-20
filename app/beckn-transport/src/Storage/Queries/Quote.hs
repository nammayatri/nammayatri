module Storage.Queries.Quote where

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
import qualified Types.Storage.Products as Product
import qualified Types.Storage.SearchReqLocation as Loc
import qualified Types.Storage.Quote as Storage
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchRequest as SearchRequest
import Types.Storage.Vehicle (Vehicle)

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.QuoteT))
getDbTable = DB.quote . DB.transporterDb <$> getSchemaName

getSRTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SearchRequest.SearchRequestT))
getSRTable =
  DB.searchRequest . DB.transporterDb <$> getSchemaName

getProdTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Product.ProductsT))
getProdTable =
  DB.products . DB.transporterDb <$> getSchemaName

getRideTable :: DBFlow m r => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Ride.RideT))
getRideTable =
  DB.ride . DB.transporterDb <$> getSchemaName

createFlow :: DBFlow m r => Storage.Quote -> m ()
createFlow =
  DB.runSqlDB . create

create :: Storage.Quote -> DB.SqlDB ()
create quote = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue quote)

findAllByIds :: DBFlow m r => Integer -> Integer -> [Id Product.Products] -> m [Storage.Quote]
findAllByIds limit offset ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Quote {..} = B.desc_ createdAt
    predicate Storage.Quote {..} =
      B.in_ productId (B.val_ <$> ids)

findAllByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = requestId ==. B.val_ quoteId

findByRequestId :: DBFlow m r => Id SearchRequest.SearchRequest -> m (Maybe Storage.Quote)
findByRequestId quoteId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} = requestId ==. B.val_ quoteId

findById' :: DBFlow m r => Id Storage.Quote -> m (Maybe Storage.Quote)
findById' quoteId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} =
      id ==. B.val_ quoteId

findAllByRequestId' :: DBFlow m r => Id SearchRequest.SearchRequest -> m [Storage.Quote]
findAllByRequestId' searchRequestId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      requestId ==. B.val_ searchRequestId

findAllByIds' :: DBFlow m r => [Id Storage.Quote] -> m [Storage.Quote]
findAllByIds' ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ id (B.val_ <$> ids)

updateStatusForProducts :: DBFlow m r => Id Product.Products -> Storage.QuoteStatus -> m ()
updateStatusForProducts productId_ status_ = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause status_ currTime)
    (predicate productId_)
  where
    predicate pId Storage.Quote {..} = productId ==. B.val_ pId
    setClause scStatus currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

updateStatusFlow ::
  DBFlow m r =>
  Id Storage.Quote ->
  Storage.QuoteStatus ->
  m ()
updateStatusFlow quoteId status = DB.runSqlDB (updateStatus quoteId status)

updateStatus ::
  Id Storage.Quote ->
  Storage.QuoteStatus ->
  DB.SqlDB ()
updateStatus quoteId status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate quoteId)
  where
    predicate pId Storage.Quote {..} =
      id ==. B.val_ pId
    setClause scStatus currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ scStatus
        ]

findAllByRequestIds :: DBFlow m r => [Id SearchRequest.SearchRequest] -> m [Storage.Quote]
findAllByRequestIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ requestId (B.val_ <$> ids)

updateStatusByIdsFlow ::
  DBFlow m r =>
  [Id Storage.Quote] ->
  Storage.QuoteStatus ->
  m ()
updateStatusByIdsFlow ids status =
  DB.runSqlDB (updateStatusByIds ids status)

updateStatusByIds ::
  [Id Storage.Quote] ->
  Storage.QuoteStatus ->
  DB.SqlDB ()
updateStatusByIds ids status_ = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause status_ currTime)
    (predicate ids)
  where
    predicate pids Storage.Quote {..} = B.in_ id (B.val_ <$> pids)
    setClause scStatus currTime' Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime',
          status <-. B.val_ scStatus
        ]

updateRequestId ::
  DBFlow m r =>
  Id Storage.Quote ->
  Id SearchRequest.SearchRequest ->
  m ()
updateRequestId quoteId searchRequestId = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause searchRequestId currTime)
    (predicate quoteId)
  where
    predicate quoteId_ Storage.Quote {..} = id ==. B.val_ quoteId_
    setClause scRequestId currTime Storage.Quote {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          requestId <-. B.val_ scRequestId
        ]

findAllByProdId :: DBFlow m r => Id Product.Products -> m [Storage.Quote]
findAllByProdId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = productId ==. B.val_ quoteId

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

findById :: DBFlow m r => Id Storage.Quote -> m (Maybe Storage.Quote)
findById pid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Quote {..} = id ==. B.val_ pid

updateDriverFlow :: DBFlow m r => [Id Storage.Quote] -> Maybe (Id Person) -> m ()
updateDriverFlow ids driverId =
  DB.runSqlDB (updateDriver ids driverId)

updateDriver ::
  [Id Storage.Quote] ->
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
    predicate pids Storage.Quote {..} = id `B.in_` (B.val_ <$> pids)
    setClause sDriverId currTime Storage.Quote {..} =
      mconcat
        [ personId <-. B.val_ sDriverId,
          personUpdatedAt <-. B.val_ (Just currTime),
          updatedAt <-. B.val_ currTime
        ]

updateVehicleFlow :: DBFlow m r => [Id Storage.Quote] -> Maybe Text -> m ()
updateVehicleFlow ids vehId = do
  DB.runSqlDB (updateVehicle ids (Id <$> vehId))

updateVehicle ::
  [Id Storage.Quote] ->
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
    predicate pids Storage.Quote {..} = id `B.in_` (B.val_ <$> pids)
    setClause vehicleId currTime' Storage.Quote {..} =
      mconcat
        [ entityId <-. B.val_ vehicleId,
          updatedAt <-. B.val_ currTime'
        ]

updateInfo :: Id Storage.Quote -> Text -> DB.SqlDB ()
updateInfo quoteId info_ = do
  dbTable <- getDbTable
  DB.update'
    dbTable
    (setClause info_)
    (predicate quoteId)
  where
    predicate quoteId_ Storage.Quote {..} = id ==. B.val_ quoteId_
    setClause pInfo Storage.Quote {..} =
      mconcat
        [info <-. B.val_ (Just pInfo)]

updateActualPrice :: Amount -> Id Storage.Quote -> DB.SqlDB ()
updateActualPrice price' prodInstId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update'
    dbTable
    (setClause price' now)
    (predicate prodInstId)
  where
    predicate piId Storage.Quote {..} = id ==. B.val_ piId
    setClause price'' currTime Storage.Quote {..} =
      mconcat
        [ actualPrice <-. B.val_ (Just price''),
          updatedAt <-. B.val_ currTime
        ]

findAllByVehicleId :: DBFlow m r => Maybe (Id Vehicle) -> m [Storage.Quote]
findAllByVehicleId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = B.val_ (isJust quoteId) &&. entityId ==. B.val_ (getId <$> quoteId)

findAllRidesWithLocationsByDriverId ::
  DBFlow m r =>
  Integer ->
  Integer ->
  Id Person ->
  m [(Storage.Quote, Loc.SearchReqLocation, Loc.SearchReqLocation)]
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
    predicate Storage.Quote {..} =
      personId ==. B.val_ (Just personId_)
    orderByDesc (Storage.Quote {..}, _, _) = B.desc_ createdAt
    
findAllByPersonId :: DBFlow m r => Id Person -> m [Storage.Quote]
findAllByPersonId quoteId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} = personId ==. B.val_ (Just quoteId)

findAllExpiredByStatus :: DBFlow m r => [Storage.QuoteStatus] -> UTCTime -> m [Storage.Quote]
findAllExpiredByStatus statuses expiryTime = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Quote {..} =
      B.in_ status (B.val_ <$> statuses)
        &&. startTime B.<=. B.val_ expiryTime

findByStartTimeBuffer ::
  DBFlow m r =>
  UTCTime ->
  NominalDiffTime ->
  [Storage.QuoteStatus] ->
  m [Storage.Quote]
findByStartTimeBuffer startTime_ buffer statuses = do
  dbTable <- getDbTable
  let fromTime = addUTCTime (- buffer * 60 * 60) startTime_
  let toTime = addUTCTime (buffer * 60 * 60) startTime_
  DB.findAll dbTable identity (predicate fromTime toTime)
  where
    predicate fromTime toTime Storage.Quote {..} =
      let inStatus = fmap B.val_ statuses
       in startTime B.<=. B.val_ toTime
            &&. startTime B.>=. B.val_ fromTime
            &&. status `B.in_` inStatus
