{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Ends a booking-fee hold on a booking nobody ever resolved.
module SharedLogic.Scheduler.Jobs.BookingDepositExpiry where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Payment as DPaymentAction
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPayment as DBP
import qualified Domain.Types.BookingStatus as DRB
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Scheduler
import qualified SharedLogic.BookingDeposit as BookingDeposit
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingPayment as QBookingPayment
import Tools.Metrics (HasBAPMetrics)
import TransactionLogs.Types (KeyConfig, TokenConfig)
import qualified UrlShortner.Common as UrlShortner

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
    HasLongDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "isMetroTestTransaction" r Bool,
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
    Nothing -> do
      BookingDeposit.releaseHolds bookingId
      logInfo $ "BookingDepositExpiry: booking never materialised, released any orphan hold: " <> bookingId.getId
      pure Complete
    Just booking | isNothing booking.bookingDepositAmount -> do
      logInfo $ "BookingDepositExpiry: booking " <> bookingId.getId <> " carries no booking fee; nothing to expire"
      pure Complete
    Just booking -> do
      repaired <- BookingDeposit.expireOrRepairBookingDeposit booking
      unless repaired $
        when (booking.status `elem` DRB.terminalBookingStatus) $ do
          holds <- BookingDeposit.findHolds booking.id
          unless (null holds) $ do
            logError $ "BookingDepositExpiry: resolving stranded hold on terminal booking " <> bookingId.getId <> " (status " <> show booking.status <> ")"
            if booking.status == DRB.COMPLETED
              then BookingDeposit.resolveTerminalHolds booking
              else
                void . withTryCatch "bookingDepositExpiry:refundStrandedHold" $
                  BookingDeposit.refundBookingDeposit booking
      when repaired $
        whenJust booking.bppBookingId $ \bppBookingId ->
          void . withTryCatch "bookingDepositExpiry:notifyBpp" $ do
            cancelRes <- DCancel.buildLocalCancelRes booking bppBookingId
            withShortRetry $ CallBPP.cancelV2 booking.merchantId booking.providerUrl =<< CancelACL.buildCancelReqV2 cancelRes Nothing
      when repaired $ do
        mbAttempt <- QBookingPayment.findLatestByBookingIdAndServiceType booking.id DOrder.BookingDeposit
        whenJust mbAttempt $ \attempt ->
          when (attempt.status == DBP.PENDING) $ do
            mbOrder <- QOrder.findById attempt.paymentOrderId
            whenJust mbOrder $ \paymentOrder -> do
              let fulfillmentHandler resp =
                    DPaymentAction.bookingDepositOrderStatusHandler paymentOrder.id booking.merchantId resp
              void . withTryCatch "bookingDepositExpiry:syncPendingAttempt" $
                SPayment.syncOrderStatus fulfillmentHandler booking.merchantId booking.riderId paymentOrder
      -- Never reschedules itself: one hold, one expiry decision.
      pure Complete
