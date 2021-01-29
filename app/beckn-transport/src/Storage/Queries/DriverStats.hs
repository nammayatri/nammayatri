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

createInitialDriverStats :: DriverId -> Flow ()
createInitialDriverStats driverId = do
  dbTable <- getDbTable
  now <- getCurrTime
  let driverStats = mkDriverStats driverId now
  DB.createOne dbTable (Storage.Common.insertExpression driverStats)
    >>= either DB.throwDBError pure
  where
    mkDriverStats id now =
      Storage.DriverStats
        { _driverId = id,
          _completedRidesNumber = 0,
          _earnings = 0.0,
          _createdAt = now,
          _updatedAt = now
        }

noOffset :: Integer -- to remove hint by hlint
noOffset = 0

findByIdsInAscendingRidesOrder :: [DriverId] -> Integer -> Flow [Storage.DriverStats]
findByIdsInAscendingRidesOrder ids limit = do
  dbTable <- getDbTable
  let order Storage.DriverStats {..} = B.asc_ _completedRidesNumber
  DB.findAllWithLimitOffsetWhere dbTable predicate limit noOffset order
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

fetchAll :: Flow [Storage.DriverStats]
fetchAll = do
  dbTable <- getDbTable
  DB.findAllRows dbTable
    >>= either DB.throwDBError pure

fetchMostOutdatedDriversStats :: Integer -> Flow [Storage.DriverStats]
fetchMostOutdatedDriversStats limit = do
  dbTable <- getDbTable
  let order Storage.DriverStats {..} = B.asc_ _updatedAt
  DB.findAllWithLimitOffset dbTable limit noOffset order
    >>= either DB.throwDBError pure

deleteById :: DriverId -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= either DB.throwDBError pure
  where
    predicate dId Storage.DriverStats {..} = _driverId ==. B.val_ dId
