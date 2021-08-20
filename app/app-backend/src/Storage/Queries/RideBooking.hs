module Storage.Queries.RideBooking where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RideBooking as Storage
import Utils.Common
import qualified Types.Storage.Quote as Quote

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