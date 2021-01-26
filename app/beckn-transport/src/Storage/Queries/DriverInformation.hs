module Storage.Queries.DriverInformation where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (DriverId)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverInformation as DriverInformation

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity DriverInformation.DriverInformationT))
getDbTable =
  DB._driverInformation . DB.transporterDb <$> getSchemaName

create :: DriverInformation.DriverInformation -> Flow ()
create DriverInformation.DriverInformation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression DriverInformation.DriverInformation {..})
    >>= either DB.throwDBError pure

findById :: DriverId -> Flow (Maybe DriverInformation.DriverInformation)
findById driverId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate DriverInformation.DriverInformation {..} = _driverId ==. B.val_ driverId

updateActivity :: DriverId -> Bool -> Flow ()
updateActivity driverId active = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause active now) (predicate driverId)
    >>= either DB.throwDBError pure
  where
    setClause a now DriverInformation.DriverInformation {..} =
      mconcat
        [ _active <-. B.val_ a,
          _updatedAt <-. B.val_ now
        ]
    predicate id DriverInformation.DriverInformation {..} = _driverId ==. B.val_ id

deleteById :: DriverId -> Flow ()
deleteById driverId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate driverId)
    >>= either DB.throwDBError pure
  where
    predicate pid DriverInformation.DriverInformation {..} = _driverId ==. B.val_ pid
