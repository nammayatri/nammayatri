module Storage.Queries.RideBookingCancellationReason where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RideBookingCancellationReason as SBCR

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SBCR.RideBookingCancellationReasonT))
getDbTable =
  DB.bookingCancellationReason . DB.transporterDb <$> getSchemaName

create :: SBCR.RideBookingCancellationReason -> DB.SqlDB ()
create rideRequest = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue rideRequest)
