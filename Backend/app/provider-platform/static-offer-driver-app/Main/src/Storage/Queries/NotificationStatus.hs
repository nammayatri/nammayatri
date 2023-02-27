{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.NotificationStatus where

import Domain.Types.Booking
import Domain.Types.NotificationStatus as NotificationStatus
import Domain.Types.Person (Driver, Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.NotificationStatus

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

deleteByPersonId :: Id Driver -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    notificationStatuses <- from $ table @NotificationStatusT
    where_ $ notificationStatuses ^. NotificationStatusDriverId ==. val (toKey . cast @Driver @Person $ personId)
