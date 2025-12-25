-- {-
--  Copyright 2022-23, Juspay India Pvt Ltd

--  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

--  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

--  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

--  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-- -}
module SharedLogic.Scheduler.Jobs.ScheduledRidePopupToRider where

import qualified Domain.Types.BookingStatus as DTB
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.Queries.Booking as QBooking
import Tools.Notifications (notifyAboutScheduledRide)

sendScheduledRidePopupToRider ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasKafkaProducer r
  ) =>
  Job 'ScheduledRidePopupToRider ->
  m ExecutionResult
sendScheduledRidePopupToRider Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
  booking <- runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  case booking.status of
    DTB.CANCELLED -> terminateJob
    DTB.REALLOCATED -> terminateJob
    _ -> do
      let title = "Scheduled Ride Reminder"
          body = "Your scheduled ride is about to start. Please be ready for the ride."
      void $ notifyAboutScheduledRide booking title body
      return Complete
  where
    terminateJob = return $ Terminate "Job Terminated because this booking is cancelled or reallocated"
