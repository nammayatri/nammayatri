module Storage.Queries.DriverStats where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount (Amount)
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (DriverId)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverStats as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverStatsT))
getDbTable =
  DB._driverStats . DB.transporterDb <$> getSchemaName

create :: Storage.DriverStats -> Flow ()
create Storage.DriverStats {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.Common.insertExpression Storage.DriverStats {..})
    >>= either DB.throwDBError pure

createInitialDriverInfo :: DriverId -> Flow ()
createInitialDriverInfo driverId = do
  dbTable <- getDbTable
  now <- getCurrTime
  let driverInfo = mkDriverInfo driverId now
  DB.createOne dbTable (Storage.Common.insertExpression driverInfo)
    >>= either DB.throwDBError pure
  where
    mkDriverInfo id now =
      Storage.DriverStats
        { _driverId = id,
          _completedRidesNumber = 0,
          _earnings = 0.0,
          _createdAt = now,
          _updatedAt = now
        }

findById :: DriverId -> Flow (Maybe Storage.DriverStats)
findById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.DriverStats {..} = _driverId ==. B.val_ id

findByIds :: [DriverId] -> Flow [Storage.DriverStats]
findByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.DriverStats {..} = _driverId `B.in_` (B.val_ <$> ids)

update :: DriverId -> Int -> Amount -> Flow ()
update driverId completedRides earnings = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause completedRides earnings now) (predicate driverId)
    >>= either DB.throwDBError pure
  where
    setClause cr e now Storage.DriverStats {..} =
      mconcat
        [ _completedRidesNumber <-. B.val_ cr,
          _earnings <-. B.val_ e,
          _updatedAt <-. B.val_ now
        ]
    predicate id Storage.DriverStats {..} = _driverId ==. B.val_ id

fetchMostOutdatedDriversInfo :: Integer -> Flow [Storage.DriverStats]
fetchMostOutdatedDriversInfo limit = do
  dbTable <- getDbTable
  let offset = 0
  let orderByDesc Storage.DriverStats {..} = B.desc_ _updatedAt
  DB.findAllWithLimitOffset dbTable limit offset orderByDesc
    >>= either DB.throwDBError pure
