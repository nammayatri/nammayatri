module Storage.Queries.AllocationRequest where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Utils.Common (getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (AllocationRequestId)
import qualified Types.Storage.AllocationRequest as AllocationRequest
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity AllocationRequest.AllocationRequestT))
getDbTable =
  DB._allocationRequest . DB.transporterDb <$> getSchemaName

create :: AllocationRequest.AllocationRequest -> Flow ()
create AllocationRequest.AllocationRequest {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression AllocationRequest.AllocationRequest {..})
    >>= either DB.throwDBError pure

fetchOldest :: Integer -> Flow [AllocationRequest.AllocationRequest]
fetchOldest limit = do
  dbTable <- getDbTable
  let noOffset = 0
  let order AllocationRequest.AllocationRequest {..} = B.asc_ _orderedAt
  DB.findAllWithLimitOffsetWhere dbTable predicate limit noOffset order
    >>= either DB.throwDBError pure
  where
    predicate AllocationRequest.AllocationRequest {..} = _status ==. B.val_ AllocationRequest.NEW

markComplete :: AllocationRequestId -> Flow ()
markComplete allocationReqId = do
  dbTable <- getDbTable
  DB.update dbTable setClause (predicate allocationReqId)
    >>= either DB.throwDBError pure
  where
    setClause AllocationRequest.AllocationRequest {..} = _status <-. B.val_ AllocationRequest.COMPLETED
    predicate id AllocationRequest.AllocationRequest {..} = _id ==. B.val_ id
