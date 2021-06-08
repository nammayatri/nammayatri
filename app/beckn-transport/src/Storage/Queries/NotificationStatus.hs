module Storage.Queries.NotificationStatus where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Data.Time (addUTCTime)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.NotificationStatus as NotificationStatus

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB.notificationStatus . DB.transporterDb <$> getSchemaName

create :: NotificationStatus.NotificationStatus -> Flow ()
create NotificationStatus.NotificationStatus {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression NotificationStatus.NotificationStatus {..})

updateStatus :: Id Ride -> Id Driver -> NotificationStatus.AnswerStatus -> Flow ()
updateStatus rideId_ driverId_ status_ = do
  dbTable <- getDbTable
  DB.update dbTable (setClause status_) (predicate rideId_ driverId_)
  where
    setClause s NotificationStatus.NotificationStatus {..} = status <-. B.val_ s
    predicate rId dId NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rId
        &&. driverId ==. B.val_ dId

fetchRefusedNotificationsByRideId :: Id Ride -> Flow [NotificationStatus.NotificationStatus]
fetchRefusedNotificationsByRideId rideId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rideId_
        &&. status `B.in_` [B.val_ NotificationStatus.REJECTED, B.val_ NotificationStatus.IGNORED]

fetchActiveNotifications :: Flow [NotificationStatus.NotificationStatus]
fetchActiveNotifications = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByRideId :: Id Ride -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByRideId rideId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rideId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByDriverId :: Id Driver -> Maybe (Id Ride) -> Flow (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByDriverId driverId_ rideId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      driverId ==. B.val_ driverId_
        &&. maybe (B.val_ True) (\v -> rideId ==. B.val_ v) rideId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

cleanupNotifications :: Id Ride -> Flow ()
cleanupNotifications rideId_ = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rideId_)
  where
    predicate rid NotificationStatus.NotificationStatus {..} = rideId ==. B.val_ rid

cleanupOldNotifications :: Flow ()
cleanupOldNotifications = do
  dbTable <- getDbTable
  compareTime <- getCurrentTime <&> addUTCTime (-300)
  DB.delete dbTable (predicate compareTime)
  where
    predicate compareTime NotificationStatus.NotificationStatus {..} = expiresAt B.<=. B.val_ compareTime
