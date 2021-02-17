module Storage.Queries.RideRequest where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (RideId, RideRequestId)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.RideRequest as RideRequest

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity RideRequest.RideRequestT))
getDbTable =
  DB._rideRequest . DB.transporterDb <$> getSchemaName

create :: RideRequest.RideRequest -> Flow ()
create RideRequest.RideRequest {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression RideRequest.RideRequest {..})
    >>= either DB.throwDBError pure

fetchOldest :: Integer -> Flow [RideRequest.RideRequest]
fetchOldest limit = do
  dbTable <- getDbTable
  let noOffset = 0
  let order RideRequest.RideRequest {..} = B.asc_ _lastProcessTime
  DB.findAllWithLimitOffset dbTable limit noOffset order
    >>= either DB.throwDBError pure

removeRide :: RideId -> Flow ()
removeRide rideId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rideId)
    >>= either DB.throwDBError pure
  where
    predicate id RideRequest.RideRequest {..} = _rideId ==. B.val_ id

updateLastProcessTime :: RideRequestId -> Flow ()
updateLastProcessTime rideReqId = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause now) (predicate rideReqId)
    >>= either DB.throwDBError pure
  where
    setClause now RideRequest.RideRequest {..} = _lastProcessTime <-. B.val_ now
    predicate id RideRequest.RideRequest {..} = _id ==. B.val_ id
