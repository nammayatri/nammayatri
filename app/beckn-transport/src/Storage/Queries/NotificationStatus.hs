module Storage.Queries.NotificationStatus where

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

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB.notificationStatus . DB.transporterDb <$> getSchemaName

createMany :: DBFlow m r => [NotificationStatus.NotificationStatus] -> m ()
createMany notificationStatus = do
  dbTable <- getDbTable
  DB.create dbTable $ Storage.insertValues notificationStatus

updateStatus :: DBFlow m r => Id Ride -> NotificationStatus.AnswerStatus -> [Id Driver] -> m ()
updateStatus rideId_ status_ driverIds = do
  dbTable <- getDbTable
  DB.update dbTable (setClause status_) (predicate rideId_ driverIds)
  where
    setClause s NotificationStatus.NotificationStatus {..} = status <-. B.val_ s
    predicate rId ids NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rId
        &&. driverId `B.in_` (B.val_ <$> ids)

fetchRefusedNotificationsByRideId :: DBFlow m r => Id Ride -> m [NotificationStatus.NotificationStatus]
fetchRefusedNotificationsByRideId rideId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rideId_
        &&. status `B.in_` [B.val_ NotificationStatus.REJECTED, B.val_ NotificationStatus.IGNORED]

fetchActiveNotifications :: DBFlow m r => m [NotificationStatus.NotificationStatus]
fetchActiveNotifications = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByRideId :: DBFlow m r => Id Ride -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRideId rideId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideId ==. B.val_ rideId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByDriverId :: DBFlow m r => Id Driver -> Maybe (Id Ride) -> m (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByDriverId driverId_ rideId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      driverId ==. B.val_ driverId_
        &&. maybe (B.val_ True) (\v -> rideId ==. B.val_ v) rideId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

cleanupNotifications :: DBFlow m r => Id Ride -> m ()
cleanupNotifications rideId_ = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rideId_)
  where
    predicate rid NotificationStatus.NotificationStatus {..} = rideId ==. B.val_ rid

cleanupOldNotifications :: DBFlow m r => m Int
cleanupOldNotifications = do
  dbTable <- getDbTable
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  rows <- DB.deleteReturning dbTable (predicate compareTime)
  return $ length rows
  where
    predicate compareTime NotificationStatus.NotificationStatus {..} = expiresAt B.<=. B.val_ compareTime
