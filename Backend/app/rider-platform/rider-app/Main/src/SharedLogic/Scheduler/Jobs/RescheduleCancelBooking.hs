{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Rider reschedule: retries a cancel the reschedule could not complete, so the rider is never
-- left with both the replaced booking and its replacement.
module SharedLogic.Scheduler.Jobs.RescheduleCancelBooking where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified SharedLogic.Reschedule as SReschedule
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Booking as QRB
import Tools.Metrics (CoreMetrics)
import TransactionLogs.Types (KeyConfig, TokenConfig)

rescheduleCancelBookingJob ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasRequestId r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  Job 'RescheduleCancelBooking ->
  m ExecutionResult
rescheduleCancelBookingJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let bookingId = jobInfo.jobData.bookingId
  mbBooking <- QRB.findById bookingId
  case mbBooking of
    Just booking | booking.status `notElem` DRB.terminalBookingStatus -> do
      res <- withTryCatch "rescheduleCancelBookingJob" $ SReschedule.cancelRescheduledBooking booking
      case res of
        Right () -> do
          logInfo $ "Reschedule: cancelled booking " <> bookingId.getId <> " on retry"
          pure Complete
        Left err -> do
          logError $ "Reschedule: cancel of booking " <> bookingId.getId <> " failed again, will retry: " <> show err
          pure Retry
    _ -> pure Complete
