{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Ends a booking-fee hold on a booking nobody ever resolved.
--
--   This is the mechanism that makes a PENDING hold stop being PENDING. Expiry is
--   deliberately not derived on read, so without this job a held fee is never released for a
--   booking the BPP never answered on.
--
--   The whole decision lives in BookingDeposit.expireOrRepairBookingDeposit, shared with the on-read
--   repair, so the two triggers can never disagree about what "expired" means -- including
--   the no-op when a ride exists. A job that cancelled an assigned ride would be far worse
--   than a stranded hold.
module SharedLogic.Scheduler.Jobs.BookingDepositExpiry where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Cancel as DCancel
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Booking as QRB
import TransactionLogs.Types (KeyConfig, TokenConfig)

bookingDepositExpiryJob ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    EncFlow m r,
    MonadMask m,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'BookingDepositExpiry ->
  m ExecutionResult
bookingDepositExpiryJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let bookingId = jobInfo.jobData.bookingId
  mbBooking <- QRB.findById bookingId
  case mbBooking of
    -- The hold is placed before createBooking writes the row, so a confirm that failed in
    -- between leaves a hold pointing at a booking that never existed. Release it: with no
    -- booking there is nothing to capture, and leaving it PENDING strands the money with no
    -- other trigger that would ever find it.
    Nothing -> do
      BookingDeposit.releaseHolds bookingId
      logInfo $ "BookingDepositExpiry: booking never materialised, released any orphan hold: " <> bookingId.getId
      pure Complete
    Just booking -> do
      repaired <- BookingDeposit.expireOrRepairBookingDeposit booking
      when repaired $
        whenJust booking.bppBookingId $ \bppBookingId ->
          void . withTryCatch "bookingDepositExpiry:notifyBpp" $ do
            cancelRes <- DCancel.buildLocalCancelRes booking bppBookingId
            withShortRetry $ CallBPP.cancelV2 booking.merchantId booking.providerUrl =<< CancelACL.buildCancelReqV2 cancelRes Nothing
      -- Never reschedules itself: one hold, one expiry decision.
      pure Complete
