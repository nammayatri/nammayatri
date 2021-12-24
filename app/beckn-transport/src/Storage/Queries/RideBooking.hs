module Storage.Queries.RideBooking where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SPers
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as Storage
import Utils.Common

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RideBookingT))
getDbTable =
  DB.rideBooking . DB.transporterDb <$> getSchemaName

getRideTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SRide.RideT))
getRideTable =
  DB.ride . DB.transporterDb <$> getSchemaName

create :: Storage.RideBooking -> DB.SqlDB ()
create rideBooking = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue rideBooking)

updateStatus :: Id Storage.RideBooking -> Storage.RideBookingStatus -> DB.SqlDB ()
updateStatus rbId rbStatus = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause rbStatus now) $ predicate rbId
  where
    predicate rbId_ Storage.RideBooking {..} = id ==. B.val_ rbId_
    setClause rbStatus_ now Storage.RideBooking {..} =
      mconcat [status <-. B.val_ rbStatus_, updatedAt <-. B.val_ now]

findById :: DBFlow m r => Id Storage.RideBooking -> m (Maybe Storage.RideBooking)
findById rbId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = id ==. B.val_ rbId

findByQuoteId :: DBFlow m r => Id Quote.Quote -> m (Maybe Storage.RideBooking)
findByQuoteId quoteId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = quoteId ==. B.val_ quoteId_

findAllByOrg :: DBFlow m r => Id Org.Organization -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.RideBooking]
findAllByOrg orgId mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy) $ predicate isOnlyActive
  where
    orderBy Storage.RideBooking {..} = B.desc_ createdAt
    predicate isOnlyActive Storage.RideBooking {..} =
      providerId ==. B.val_ orgId
        &&. B.not_ (status ==. B.val_ Storage.CONFIRMED)
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True

findAllByDriver :: DBFlow m r => Id SPers.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.RideBooking]
findAllByDriver driverId_ mbLimit mbOffset mbIsOnlyActive = do
  dbTable <- getDbTable
  rideTable <- getRideTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbIsOnlyActive
  DB.findAllByJoin (B.limit_ limit . B.offset_ offset . B.orderBy_ orderBy) $ query dbTable rideTable isOnlyActive
  where
    orderBy Storage.RideBooking {..} = B.desc_ createdAt
    query dbTable rideTable isOnlyActive = do
      rideBooking <- B.all_ dbTable
      ride <- B.join_ rideTable $ \row ->
        row.bookingId ==. rideBooking.id
      B.guard_ $ predicate ride isOnlyActive
      return rideBooking
    predicate SRide.Ride {..} isOnlyActive =
      driverId ==. B.val_ driverId_
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ SRide.COMPLETED ||. status ==. B.val_ SRide.CANCELLED)
          else B.val_ True
