{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckMultimodalConfirmFail where

import qualified Domain.Types.FRFSTicketBookingPayment as DFRFSTicketBookingPayment
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBooking
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import SharedLogic.Payment as SPayment
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment
import Tools.Metrics (HasBAPMetrics)
import qualified UrlShortner.Common as UrlShortner

checkMultimodalConfirmFailJob ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'CheckMultimodalConfirmFail ->
  m ExecutionResult
checkMultimodalConfirmFailJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (InvalidRequest $ "booking not found for id: " <> show bookingId)
  frfsTickets <- QFRFSTicket.findAllByTicketBookingId bookingId

  paymentBooking <- QFRFSTicketBookingPayment.findTicketBookingPayment booking >>= fromMaybeM (InvalidRequest $ "payment booking not found for booking id: " <> show bookingId)

  let isPaymentInTerminalState = paymentBooking.status == DFRFSTicketBookingPayment.SUCCESS || paymentBooking.status == DFRFSTicketBookingPayment.REFUND_PENDING

  if ((booking.status == DFRFSTicketBooking.FAILED || null frfsTickets) && isPaymentInTerminalState)
    then do
      void $ SPayment.markRefundPendingAndSyncOrderStatus booking.merchantId booking.riderId paymentBooking.paymentOrderId
      return Complete
    else return Complete
