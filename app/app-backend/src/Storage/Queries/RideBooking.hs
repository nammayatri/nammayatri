module Storage.Queries.RideBooking where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Person as SPers
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.RideBooking as Storage
import qualified Types.Storage.SearchRequest as SSR
import Utils.Common

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.RideBookingT))
getDbTable =
  DB.rideBooking . DB.appDb <$> getSchemaName

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

updateBPPBookingId :: Id Storage.RideBooking -> Id Storage.BPPRideBooking -> DB.SqlDB ()
updateBPPBookingId rbId bppRbId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update' dbTable (setClause bppRbId now) $ predicate rbId
  where
    predicate rbId_ Storage.RideBooking {..} = id ==. B.val_ rbId_
    setClause bppRbId_ now Storage.RideBooking {..} =
      mconcat [bppBookingId <-. B.val_ (Just bppRbId_), updatedAt <-. B.val_ now]

findById :: DBFlow m r => Id Storage.RideBooking -> m (Maybe Storage.RideBooking)
findById rbId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = id ==. B.val_ rbId

findByBPPBookingId :: DBFlow m r => Id Storage.BPPRideBooking -> m (Maybe Storage.RideBooking)
findByBPPBookingId bppRbId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = bppBookingId ==. B.val_ (Just bppRbId)

findByQuoteId :: DBFlow m r => Id Quote.Quote -> m (Maybe Storage.RideBooking)
findByQuoteId quoteId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = quoteId ==. B.val_ quoteId_

findByRequestId :: DBFlow m r => Id SSR.SearchRequest -> m (Maybe Storage.RideBooking)
findByRequestId searchRequestId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RideBooking {..} = requestId ==. B.val_ searchRequestId

findAllByRequestorId :: DBFlow m r => Id SPers.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Storage.RideBooking]
findAllByRequestorId personId mbLimit mbOffset mbOnlyActive = do
  dbTable <- getDbTable
  let limit = fromMaybe 0 mbLimit
      offset = fromMaybe 0 mbOffset
      isOnlyActive = Just True == mbOnlyActive
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset) $ predicate isOnlyActive
  where
    predicate isOnlyActive Storage.RideBooking {..} =
      requestorId ==. B.val_ personId
        &&. if isOnlyActive
          then B.not_ (status ==. B.val_ Storage.COMPLETED ||. status ==. B.val_ Storage.CANCELLED)
          else B.val_ True