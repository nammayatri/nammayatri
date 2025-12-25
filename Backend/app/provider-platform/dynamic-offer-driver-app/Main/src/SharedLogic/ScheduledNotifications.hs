{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ScheduledNotifications where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()

pushReminderUpdatesInScheduler :: (MonadFlow m, SchedulerFlow r, EsqDBFlow m r, CacheFlow m r) => DB.Booking -> DR.Ride -> UTCTime -> Id DP.Person -> DRN.RideRelatedNotificationConfig -> m ()
pushReminderUpdatesInScheduler booking ride now driverId DRN.RideRelatedNotificationConfig {..} = do
  let eventAt = case timeDiffEvent of
        DRN.RIDE_ASSIGNED -> ride.createdAt -- gives the time of TRIP_ASSIGNED
        DRN.PICKUP_TIME -> booking.startTime -- scheduled time
        DRN.START_TIME -> fromMaybe now ride.tripStartTime
        DRN.END_TIME -> fromMaybe now ride.tripEndTime
  let currentTimeDiffFromEventTime = diffUTCTime eventAt now
      toSchedule = if onScheduledBooking then booking.isScheduled else True
  when (toSchedule && (eventTime /= DRN.PreEvent || currentTimeDiffFromEventTime >= timeDiff)) do
    let scheduleAfter = if eventTime == DRN.PostEvent then (currentTimeDiffFromEventTime + timeDiff) else (currentTimeDiffFromEventTime - timeDiff)
        dfCalculationJobTs = max 2 scheduleAfter -- Buffer of 2 seconds in case of <=0 timeDiff
    createJobIn @_ @'ScheduledRideNotificationsToDriver (Just booking.providerId) (Just booking.merchantOperatingCityId) dfCalculationJobTs $
      ScheduledRideNotificationsToDriverJobData
        { merchantId = booking.providerId,
          merchantOperatingCityId = booking.merchantOperatingCityId,
          timeDiffEvent = timeDiffEvent,
          bookingStatus = onBookingStatus,
          notificationType = notificationType,
          notificationKey = notificationKey,
          onlyIfOffline = onlyIfOffline,
          bookingId = booking.id,
          driverId = driverId
        }
