module Storage.Queries.NotificationStatus where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.DB.Types as DB
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.NotificationStatus as NotificationStatus
import Utils.Common

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB._notificationStatus . DB.transporterDb <$> getSchemaName

create :: NotificationStatus.NotificationStatus -> Flow ()
create NotificationStatus.NotificationStatus {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression NotificationStatus.NotificationStatus {..})
    >>= checkDBError

updateStatus :: Id Ride -> Id Driver -> NotificationStatus.AnswerStatus -> Flow ()
updateStatus rideId driverId status = do
  dbTable <- getDbTable
  DB.update dbTable (setClause status) (predicate rideId driverId)
    >>= checkDBError
  where
    setClause s NotificationStatus.NotificationStatus {..} = _status <-. B.val_ s
    predicate rId dId NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rId
        &&. _driverId ==. B.val_ dId

fetchRefusedNotificationsByRideId :: Id Ride -> Flow [NotificationStatus.NotificationStatus]
fetchRefusedNotificationsByRideId rideId = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rideId
        &&. _status `B.in_` [B.val_ NotificationStatus.REJECTED, B.val_ NotificationStatus.IGNORED]

fetchActiveNotifications :: Flow [NotificationStatus.NotificationStatus]
fetchActiveNotifications = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByRideId :: Id Ride -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByRideId rideId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _rideId ==. B.val_ rideId
        &&. _status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByDriverId :: Id Driver -> Maybe (Id Ride) -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByDriverId driverId rideId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate NotificationStatus.NotificationStatus {..} =
      _driverId ==. B.val_ driverId
        &&. maybe (B.val_ True) (\v -> _rideId ==. B.val_ v) rideId
        &&. _status ==. B.val_ NotificationStatus.NOTIFIED

cleanupNotifications :: Id Ride -> Flow ()
cleanupNotifications rideId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rideId)
    >>= checkDBError
  where
    predicate id NotificationStatus.NotificationStatus {..} = _rideId ==. B.val_ id
