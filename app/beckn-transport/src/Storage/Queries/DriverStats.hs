{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.DriverStats where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import Types.Error
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverStats as Storage

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverStatsT))
getDbTable = DB._driverStats . DB.transporterDb <$> getSchemaName

createInitialDriverStats :: Id Driver -> Flow ()
createInitialDriverStats driverId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  let driverStats =
        Storage.DriverStats
          { _driverId = driverId,
            _idleSince = now
          }
  DB.createOne dbTable (Storage.Common.insertExpression driverStats)
    >>= checkDBError

getFirstDriverInTheQueue :: [Id Driver] -> Flow (Id Driver)
getFirstDriverInTheQueue ids = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate 1 0 order
    >>= checkDBError
    >>= fromMaybeM EmptyDriverPool . listToMaybe . map (^. #_driverId)
  where
    predicate Storage.DriverStats {..} = _driverId `B.in_` (B.val_ <$> ids)
    order Storage.DriverStats {..} = B.asc_ _idleSince

-- TODO: delete in favour of transactional version
updateIdleTimeFlow :: Id Driver -> Flow ()
updateIdleTimeFlow = DB.runSqlDB . updateIdleTime >=> checkDBError

updateIdleTime :: Id Driver -> DB.SqlDB ()
updateIdleTime driverId = do
  dbTable <- getDbTable
  now <- asks DB.currentTime
  DB.update' dbTable (setClause now) (predicate driverId)
  where
    setClause now Storage.DriverStats {..} =
      mconcat
        [ _idleSince <-. B.val_ now
        ]
    predicate id Storage.DriverStats {..} = _driverId ==. B.val_ id

fetchAll :: Flow [Storage.DriverStats]
fetchAll = do
  dbTable <- getDbTable
  DB.findAllRows dbTable
    >>= checkDBError

deleteById :: Id Driver -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= checkDBError
  where
    predicate dId Storage.DriverStats {..} = _driverId ==. B.val_ dId
