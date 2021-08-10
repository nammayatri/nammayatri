module Storage.Queries.RideCancellationReason where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Schema
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RideCancellationReason as SRCR

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity SRCR.RideCancellationReasonT))
getDbTable =
  DB.rideCancellationReason . DB.transporterDb <$> getSchemaName

create :: SRCR.RideCancellationReason -> DB.SqlDB ()
create rideRequest = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertValue rideRequest)