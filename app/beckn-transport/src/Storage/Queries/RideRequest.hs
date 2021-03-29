module Storage.Queries.RideRequest where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RideRequest as RideRequest

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity RideRequest.RideRequestT))
getDbTable =
  DB._rideRequest . DB.transporterDb <$> getSchemaName

create :: RideRequest.RideRequest -> Flow ()
create RideRequest.RideRequest {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression RideRequest.RideRequest {..})
    >>= either throwDBError pure

fetchOldest :: Integer -> Flow [RideRequest.RideRequest]
fetchOldest limit = do
  dbTable <- getDbTable
  let noOffset = 0
  let order RideRequest.RideRequest {..} = B.asc_ _createdAt
  DB.findAllWithLimitOffset dbTable limit noOffset order
    >>= either throwDBError pure

removeRequest :: Id RideRequest.RideRequest -> Flow ()
removeRequest requestId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate requestId)
    >>= either throwDBError pure
  where
    predicate id RideRequest.RideRequest {..} = _id ==. B.val_ id
