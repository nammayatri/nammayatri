module Storage.Queries.NotificationStatus where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.NotificationStatus as NotificationStatus
import qualified Types.Storage.RideBooking as SRB

getDbTable :: (Functor m, HasSchemaName m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity NotificationStatus.NotificationStatusT))
getDbTable =
  DB.notificationStatus . DB.transporterDb <$> getSchemaName

createMany :: DBFlow m r => [NotificationStatus.NotificationStatus] -> m ()
createMany notificationStatus = do
  dbTable <- getDbTable
  DB.create dbTable $ Storage.insertValues notificationStatus

updateStatus :: DBFlow m r => Id SRB.RideBooking -> NotificationStatus.AnswerStatus -> [Id Driver] -> m ()
updateStatus rideBookingId_ status_ driverIds = do
  dbTable <- getDbTable
  DB.update dbTable (setClause status_) (predicate rideBookingId_ driverIds)
  where
    setClause s NotificationStatus.NotificationStatus {..} = status <-. B.val_ s
    predicate rId ids NotificationStatus.NotificationStatus {..} =
      rideBookingId ==. B.val_ rId
        &&. driverId `B.in_` (B.val_ <$> ids)

fetchRefusedNotificationsByRideId :: DBFlow m r => Id SRB.RideBooking -> m [NotificationStatus.NotificationStatus]
fetchRefusedNotificationsByRideId rideBookingId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideBookingId ==. B.val_ rideBookingId_
        &&. status `B.in_` [B.val_ NotificationStatus.REJECTED, B.val_ NotificationStatus.IGNORED]

fetchActiveNotifications :: DBFlow m r => m [NotificationStatus.NotificationStatus]
fetchActiveNotifications = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByRideId :: DBFlow m r => Id SRB.RideBooking -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRideId rideBookingId_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      rideBookingId ==. B.val_ rideBookingId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

findActiveNotificationByDriverId :: DBFlow m r => Id Driver -> Id SRB.RideBooking -> m (Maybe NotificationStatus.NotificationStatus)
findActiveNotificationByDriverId driverId_ rideBookingId_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate NotificationStatus.NotificationStatus {..} =
      driverId ==. B.val_ driverId_
        &&. rideBookingId ==. B.val_ rideBookingId_
        &&. status ==. B.val_ NotificationStatus.NOTIFIED

cleanupNotifications :: DBFlow m r => Id SRB.RideBooking -> m ()
cleanupNotifications rideBookingId_ = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rideBookingId_)
  where
    predicate rid NotificationStatus.NotificationStatus {..} = rideBookingId ==. B.val_ rid

cleanupOldNotifications :: DBFlow m r => m Int
cleanupOldNotifications = do
  dbTable <- getDbTable
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  rows <- DB.deleteReturning dbTable (predicate compareTime)
  return $ length rows
  where
    predicate compareTime NotificationStatus.NotificationStatus {..} = expiresAt B.<=. B.val_ compareTime
