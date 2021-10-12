module Storage.Queries.DriverStats where

import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverStats as Storage
import Utils.Common

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverStatsT))
getDbTable = DB.driverStats . DB.transporterDb <$> getSchemaName

createInitialDriverStats :: Id Driver -> DB.SqlDB ()
createInitialDriverStats driverId_ = do
  dbTable <- getDbTable
  now <- getCurrentTime
  let driverStats =
        Storage.DriverStats
          { driverId = driverId_,
            idleSince = now
          }
  DB.createOne' dbTable (Storage.Common.insertValue driverStats)

getTopDriversByIdleTime :: Int -> DBFlow m r => [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count ids = do
  dbTable <- getDbTable
  let integerCount = fromIntegral count
  drivers <- DB.findAll dbTable (B.limit_ integerCount . B.orderBy_ order) predicate
  pure $ map (.driverId) drivers
  where
    predicate Storage.DriverStats {..} = driverId `B.in_` (B.val_ <$> ids)
    order Storage.DriverStats {..} = B.asc_ idleSince

-- TODO: delete in favour of transactional versions
updateIdleTimeFlow :: DBFlow m r => Id Driver -> m ()
updateIdleTimeFlow = DB.runSqlDB . updateIdleTime

updateIdleTimesFlow :: DBFlow m r => [Id Driver] -> m ()
updateIdleTimesFlow = DB.runSqlDB . updateIdleTimes

updateIdleTime :: Id Driver -> DB.SqlDB ()
updateIdleTime driverId_ = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause now) (predicate driverId_)
  where
    setClause now Storage.DriverStats {..} =
      mconcat
        [ idleSince <-. B.val_ now
        ]
    predicate id Storage.DriverStats {..} = driverId ==. B.val_ id

updateIdleTimes :: [Id Driver] -> DB.SqlDB ()
updateIdleTimes driverIds = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause now) (predicate driverIds)
  where
    setClause now Storage.DriverStats {..} =
      mconcat
        [ idleSince <-. B.val_ now
        ]
    predicate ids Storage.DriverStats {..} = driverId `B.in_` (B.val_ <$> ids)

fetchAll :: DBFlow m r => m [Storage.DriverStats]
fetchAll = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (const (B.val_ True))

deleteById :: Id Driver -> DB.SqlDB ()
deleteById driverId_ = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate driverId_)
  where
    predicate dId Storage.DriverStats {..} = driverId ==. B.val_ dId
