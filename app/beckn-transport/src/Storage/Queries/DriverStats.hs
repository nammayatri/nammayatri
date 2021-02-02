{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.DriverStats where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
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
  let driverStats =
        Storage.DriverStats
          { _driverId = driverId,
            _idleSince = now
          }
  DB.createOne dbTable (Storage.Common.insertExpression driverStats)
    >>= either DB.throwDBError pure

noOffset, noLimit :: Integer -- to remove hint by hlint
noOffset = 0
noLimit = 0

sortByIdleTime :: [DriverId] -> Flow [DriverId]
sortByIdleTime ids = do
  dbTable <- getDbTable
  let order Storage.DriverStats {..} = B.asc_ _idleSince
  DB.findAllWithLimitOffsetWhere dbTable predicate noLimit noOffset order
    >>= either DB.throwDBError pure
    <&> map (^. #_driverId)
  where
    predicate Storage.DriverStats {..} = _driverId `B.in_` (B.val_ <$> ids)

update :: DriverId -> Flow ()
update driverId = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause now) (predicate driverId)
    >>= either DB.throwDBError pure
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
    >>= either DB.throwDBError pure

deleteById :: DriverId -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= either DB.throwDBError pure
  where
    predicate dId Storage.DriverStats {..} = _driverId ==. B.val_ dId
