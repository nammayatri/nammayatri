module Storage.Queries.DriverStats where

import App.Types (Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import Types.Error
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverStats as Storage
import Utils.Common

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverStatsT))
getDbTable = DB.driverStats . DB.transporterDb <$> getSchemaName

createInitialDriverStats :: Id Driver -> Flow ()
createInitialDriverStats driverId_ = do
  dbTable <- getDbTable
  now <- getCurrentTime
  let driverStats =
        Storage.DriverStats
          { driverId = driverId_,
            idleSince = now
          }
  DB.createOne dbTable (Storage.Common.insertExpression driverStats)

getFirstDriverInTheQueue :: [Id Driver] -> Flow (Id Driver)
getFirstDriverInTheQueue ids = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ 1 . B.orderBy_ order) predicate
    >>= fromMaybeM EmptyDriverPool . listToMaybe . map (.driverId)
  where
    predicate Storage.DriverStats {..} = driverId `B.in_` (B.val_ <$> ids)
    order Storage.DriverStats {..} = B.asc_ idleSince

-- TODO: delete in favour of transactional version
updateIdleTimeFlow :: Id Driver -> Flow ()
updateIdleTimeFlow = DB.runSqlDB . updateIdleTime

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

fetchAll :: Flow [Storage.DriverStats]
fetchAll = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (const (B.val_ True))

deleteById :: Id Driver -> DB.SqlDB ()
deleteById driverId_ = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate driverId_)
  where
    predicate dId Storage.DriverStats {..} = driverId ==. B.val_ dId
