module Storage.Queries.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Booking
import Domain.Types.NotificationStatus as NotificationStatus
import Storage.Tabular.NotificationStatus
import Types.App
import Utils.Common

createMany :: [NotificationStatus] -> SqlDB ()
createMany = Esq.createMany

updateStatus :: Id Booking -> AnswerStatus -> [Id Driver] -> SqlDB ()
updateStatus bookingId status driverIds =
  Esq.update $ \tbl -> do
    set
      tbl
      [ NotificationStatusStatus =. val status
      ]
    where_ $
      tbl ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. tbl ^. NotificationStatusDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAttemptedNotificationsByRBId :: Transactionable m => Id Booking -> m [NotificationStatus]
fetchAttemptedNotificationsByRBId bookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. notificationStatus ^. NotificationStatusStatus `in_` valList [NotificationStatus.REJECTED, NotificationStatus.IGNORED]
    return notificationStatus

fetchActiveNotifications :: Transactionable m => m [NotificationStatus]
fetchActiveNotifications =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByRBId :: Transactionable m => Id Booking -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRBId bookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByDriverId :: Transactionable m => Id Driver -> Id Booking -> m (Maybe NotificationStatus)
findActiveNotificationByDriverId driverId bookingId =
  Esq.findOne $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
        &&. notificationStatus ^. NotificationStatusDriverId ==. val (toKey $ cast driverId)
    return notificationStatus

cleanupNotifications :: Id Booking -> SqlDB ()
cleanupNotifications bookingId =
  Esq.delete $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId))

cleanupOldNotifications :: SqlDB Int
cleanupOldNotifications = do
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  res <- Esq.deleteReturningCount $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusExpiresAt ==. val compareTime)
  return $ fromIntegral res
