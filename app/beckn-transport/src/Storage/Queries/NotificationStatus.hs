module Storage.Queries.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.NotificationStatus as NotificationStatus
import Domain.Types.RideBooking
import Storage.Tabular.NotificationStatus
import Types.App
import Utils.Common

createMany :: [NotificationStatus] -> SqlDB ()
createMany = createMany'

updateStatus :: Id RideBooking -> AnswerStatus -> [Id Driver] -> SqlDB ()
updateStatus rideBookingId status driverIds =
  update' $ \tbl -> do
    set
      tbl
      [ NotificationStatusStatus =. val status
      ]
    where_ $
      tbl ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. tbl ^. NotificationStatusDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAttemptedNotificationsByRBId :: Transactionable m => Id RideBooking -> m [NotificationStatus]
fetchAttemptedNotificationsByRBId rideBookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus `in_` valList [NotificationStatus.REJECTED, NotificationStatus.IGNORED]
    return notificationStatus

fetchActiveNotifications :: Transactionable m => m [NotificationStatus]
fetchActiveNotifications =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByRBId :: Transactionable m => Id RideBooking -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRBId rideBookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByDriverId :: Transactionable m => Id Driver -> Id RideBooking -> m (Maybe NotificationStatus)
findActiveNotificationByDriverId driverId rideBookingId =
  Esq.findOne $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
        &&. notificationStatus ^. NotificationStatusDriverId ==. val (toKey $ cast driverId)
    return notificationStatus

cleanupNotifications :: Id RideBooking -> SqlDB ()
cleanupNotifications rideBookingId =
  Esq.delete' $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusRideBookingId ==. val (toKey rideBookingId))

cleanupOldNotifications :: SqlDB Int
cleanupOldNotifications = do
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  res <- Esq.deleteReturningCount' $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusExpiresAt ==. val compareTime)
  return $ fromIntegral res
