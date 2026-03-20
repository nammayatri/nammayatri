{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.PaymentOrderStatusCheck where

import qualified Domain.Action.UI.BBPS as BBPS
import qualified Domain.Action.UI.FRFSTicketService as FRFSTicketService
import qualified Domain.Action.UI.ParkingBooking as ParkingBooking
import qualified Domain.Action.UI.Pass as Pass
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.RideStatus as DRideStatus
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

paymentOrderStatusCheckJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'PaymentOrderStatusCheck ->
  m ExecutionResult
paymentOrderStatusCheckJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId
  now <- getCurrentTime
  -- Extended lookback from 1 hour to 6 hours for better reconciliation coverage.
  -- Payments completing after 1 hour were previously never discovered by backend.
  allRecentNonTerminalOrders <- QPaymentOrder.findAllNonTerminalOrders (addUTCTime (intToNominalDiffTime (-21600)) now)

  logInfo $ "Found " <> show (length allRecentNonTerminalOrders) <> " payment orders with non-terminal status within the last 6 hours"

  mapM_
    ( \paymentOrder -> do
        result <-
          withTryCatch "paymentOrderStatusCheckJob:processPaymentOrder" $
            processPaymentOrder merchantId' merchantOperatingCityId' paymentOrder
        case result of
          Left err -> do
            logError $ "Payment order status check failed for order " <> paymentOrder.id.getId <> ": " <> show err
          Right _ -> do
            logInfo $ "Payment order status check succeeded for order " <> paymentOrder.id.getId
    )
    allRecentNonTerminalOrders

  -- Reconciliation pass: find CHARGED orders where ride was cancelled/failed but no refund initiated.
  -- This catches "payment deducted but ride cancelled" cases that were missed by the real-time flow.
  reconciledCount <- reconcileChargedButCancelledOrders allRecentNonTerminalOrders
  when (reconciledCount > 0) $
    logInfo $ "Reconciliation: auto-initiated refunds for " <> show reconciledCount <> " charged-but-cancelled orders"

  logInfo $ "Completed payment order status check for " <> show (length allRecentNonTerminalOrders) <> " orders"

  -- Schedule the next run in 1 hour
  let newJobData =
        PaymentOrderStatusCheckJobData
          { merchantId = merchantId',
            merchantOperatingCityId = merchantOperatingCityId'
          }
  createJobIn @_ @'PaymentOrderStatusCheck (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 3600) newJobData
  logInfo $ "Scheduled next payment order status check job to run in 1 hour"
  return Complete

processPaymentOrder ::
  forall m r c.
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    CallFRFSBPP.BecknAPICallFlow m r,
    HasFlowEnv m r '["googleSAPrivateKey" ::: String],
    Metrics.HasBAPMetrics m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig],
    HasField "ltsHedisEnv" r Redis.HedisEnv,
    HasField "secondaryLTSHedisEnv" r (Maybe Redis.HedisEnv),
    HasField "cloudType" r (Maybe CloudType),
    HasField "isMetroTestTransaction" r Bool,
    HasField "blackListedJobs" r [Text]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DOrder.PaymentOrder ->
  m ()
processPaymentOrder merchantId merchantOperatingCityId paymentOrder = do
  person <- QPerson.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
  let paymentServiceType = fromMaybe Payment.FRFSMultiModalBooking paymentOrder.paymentServiceType
      orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
      fulfillmentHandler = mkFulfillmentHandler paymentServiceType (cast merchantId) paymentOrder.id
  void $ SPayment.orderStatusHandler merchantOperatingCityId fulfillmentHandler paymentServiceType paymentOrder orderStatusCall
  where
    -- Helper to create fulfillment handler based on payment service type
    mkFulfillmentHandler :: Payment.PaymentServiceType -> Id DM.Merchant -> Id DOrder.PaymentOrder -> DPayment.PaymentStatusResp -> m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
    mkFulfillmentHandler serviceType mId orderId paymentStatusResp = case serviceType of
      Payment.FRFSBooking -> FRFSTicketService.frfsOrderStatusHandler mId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      Payment.FRFSBusBooking -> FRFSTicketService.frfsOrderStatusHandler mId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      Payment.FRFSMultiModalBooking -> FRFSTicketService.frfsOrderStatusHandler mId paymentStatusResp JMU.switchFRFSQuoteTierUtil
      Payment.FRFSPassPurchase -> do
        status <- DPayment.getTransactionStatus paymentStatusResp
        Pass.passOrderStatusHandler orderId mId status
      Payment.ParkingBooking -> do
        status <- DPayment.getTransactionStatus paymentStatusResp
        ParkingBooking.parkingBookingOrderStatusHandler orderId mId status
      Payment.BBPS -> do
        paymentFulfillStatus <- BBPS.bbpsOrderStatusHandler mId paymentStatusResp
        pure (paymentFulfillStatus, Nothing, Nothing)
      _ -> pure (DPayment.FulfillmentPending, Nothing, Nothing)

-- | Reconciliation pass: detect CHARGED payment orders where the associated ride
-- has been cancelled or failed, and auto-initiate refunds.
-- This catches the "payment deducted but ride cancelled" race condition that was
-- missed by the real-time cancellation flow (e.g., payment completed between
-- the cancellation check and the actual cancel).
reconcileChargedButCancelledOrders ::
  forall m r.
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    HasField "blackListedJobs" r [Text]
  ) =>
  [DOrder.PaymentOrder] ->
  m Int
reconcileChargedButCancelledOrders nonTerminalOrders = do
  -- Filter to CHARGED orders with FulfillmentPending (payment succeeded but fulfillment didn't complete)
  let chargedPendingOrders =
        filter
          ( \order ->
              order.status == Payment.CHARGED
                && order.paymentFulfillmentStatus `elem` [Just DPayment.FulfillmentPending, Just DPayment.FulfillmentFailed, Nothing]
          )
          nonTerminalOrders
  logInfo $ "Reconciliation: checking " <> show (length chargedPendingOrders) <> " CHARGED orders with pending/failed fulfillment"
  foldM processReconOrder 0 chargedPendingOrders
  where
    processReconOrder :: Int -> DOrder.PaymentOrder -> m Int
    processReconOrder count order = do
      result <- withTryCatch "reconcileChargedButCancelled" $ do
        -- Find the invoice linked to this payment order to get the rideId
        mbInvoice <- QPaymentInvoice.findByPaymentOrderIdAndInvoiceType (Just order.id) DPI.PAYMENT
        case mbInvoice of
          Nothing -> return count
          Just invoice -> do
            mbRide <- QRide.findById invoice.rideId
            case mbRide of
              Nothing -> return count
              Just ride -> do
                -- If ride is CANCELLED but payment was CHARGED, trigger auto-refund
                if ride.status == DRideStatus.CANCELLED
                  then do
                    -- Use Redis dedup key to avoid re-processing on every job run
                    let reconDedupKey = "PaymentRecon:AutoRefund:OrderId-" <> order.id.getId
                    alreadyProcessed <- Redis.get @Text reconDedupKey
                    case alreadyProcessed of
                      Just _ -> return count -- already processed
                      Nothing -> do
                        Redis.setExp reconDedupKey ("1" :: Text) 86400 -- 24h TTL
                        logInfo $ "Reconciliation: ride " <> invoice.rideId.getId <> " is CANCELLED but payment order " <> order.id.getId <> " is CHARGED. Initiating auto-refund."
                        void $ SPayment.initiateRefundWithPaymentStatusRespSync (cast order.personId) order.id
                        logInfo $ "Reconciliation: auto-refund initiated for order " <> order.id.getId
                        return (count + 1)
                  else return count
      case result of
        Left err -> do
          logError $ "Reconciliation failed for order " <> order.id.getId <> ": " <> show err
          return count
        Right c -> return c
