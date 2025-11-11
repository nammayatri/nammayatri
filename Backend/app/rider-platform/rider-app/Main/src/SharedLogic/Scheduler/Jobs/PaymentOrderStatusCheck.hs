{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.PaymentOrderStatusCheck where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson
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
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  Job 'PaymentOrderStatusCheck ->
  m ExecutionResult
paymentOrderStatusCheckJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId

  let notFailedPaymentStatuses = [Payment.NEW, Payment.PENDING_VBV, Payment.CHARGED, Payment.AUTHORIZING, Payment.COD_INITIATED, Payment.STARTED, Payment.AUTO_REFUNDED]
      notSuccessfulPaymentFulfillmentStatuses = [Just DPayment.FulfillmentPending, Just DPayment.FulfillmentFailed, Just DPayment.FulfillmentRefundPending, Just DPayment.FulfillmentRefundInitiated, Nothing]

  allRecentOrders <- QPaymentOrder.findAllValidOrders notFailedPaymentStatuses notSuccessfulPaymentFulfillmentStatuses
  recentNotFailedOrNotChargedOrders <-
    filterM
      ( \order ->
          case order.status of
            Payment.CHARGED -> do
              refunds <- QRefunds.findAllByOrderId order.shortId
              let hasPendingRefund = any (\refund -> refund.status == Payment.REFUND_PENDING || refund.status == Payment.MANUAL_REVIEW) refunds
              return hasPendingRefund
            _ -> return True
      )
      allRecentOrders

  logInfo $ "Found " <> show (length recentNotFailedOrNotChargedOrders) <> " payment orders with status not CHARGED (or CHARGED with pending refunds) created within the last hour"

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
    recentNotFailedOrNotChargedOrders

  logInfo $ "Completed payment order status check for " <> show (length recentNotFailedOrNotChargedOrders) <> " orders"

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
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DOrder.PaymentOrder ->
  m ()
processPaymentOrder merchantId merchantOperatingCityId paymentOrder = do
  person <- QPerson.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
  let paymentServiceType = fromMaybe Payment.FRFSMultiModalBooking paymentOrder.paymentServiceType
      orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
      commonPersonId = cast @DP.Person @DPayment.Person person.id

  paymentStatusResponse <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
  void $ SPayment.orderStatusHandler paymentServiceType paymentOrder paymentStatusResponse
