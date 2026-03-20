{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckOrphanedPassPayment where

import qualified Domain.Action.UI.Pass as Pass
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPurchasedPassPayment
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

-- | Scheduled job that finds payment attempts stuck in PENDING state for > 30 minutes
-- and reconciles them by checking payment status at Juspay.
--
-- For each orphaned payment:
--   - If payment succeeded at Juspay → activate the pass (via passOrderStatusHandler)
--   - If payment failed at Juspay → mark payment as Failed
--   - If payment not found → mark payment as Failed
--
-- This is the pass equivalent of CheckMultimodalConfirmFail for tickets.
-- Runs every 30 minutes and self-schedules the next run.
checkOrphanedPassPaymentJob ::
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
  Job 'CheckOrphanedPassPayment ->
  m ExecutionResult
checkOrphanedPassPaymentJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId

  now <- getCurrentTime
  -- Find payment attempts in PENDING state created more than 30 minutes ago
  let cutoffTime = addUTCTime (intToNominalDiffTime (-1800)) now
  orphanedPayments <- QPurchasedPassPayment.findAllPendingPaymentsOlderThan cutoffTime (Just 100)

  logInfo $ "Found " <> show (length orphanedPayments) <> " orphaned pass payments in PENDING state older than 30 minutes"

  mapM_ (processOrphanedPayment merchantId' merchantOperatingCityId') orphanedPayments

  -- Self-schedule next run in 30 minutes
  let newJobData =
        CheckOrphanedPassPaymentJobData
          { merchantId = merchantId',
            merchantOperatingCityId = merchantOperatingCityId'
          }
  createJobIn @_ @'CheckOrphanedPassPayment (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 1800) newJobData
  logInfo "Scheduled next orphaned pass payment check job in 30 minutes"
  return Complete

processOrphanedPayment ::
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
  DPurchasedPassPayment.PurchasedPassPayment ->
  m ()
processOrphanedPayment merchantId merchantOperatingCityId payment = do
  result <-
    withTryCatch "checkOrphanedPassPayment:processOrphanedPayment" $ do
      -- Check status at payment gateway via the payment order
      mbPaymentOrder <- QPaymentOrder.findById payment.orderId
      case mbPaymentOrder of
        Nothing -> do
          logWarning $ "No payment order found for orphaned payment " <> payment.id.getId <> " orderId " <> payment.orderId.getId <> ", marking payment as Failed"
          QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.Failed payment.orderId
        Just paymentOrder -> do
          -- Use the existing payment order status handler to reconcile
          -- This will call passOrderStatusHandler which updates both pass and payment
          person <- QPerson.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
          let orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId Nothing Payment.FRFSPassPurchase (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
              fulfillmentHandler paymentStatusResp = do
                status <- DPayment.getTransactionStatus paymentStatusResp
                Pass.passOrderStatusHandler paymentOrder.id merchantId status
          void $ SPayment.orderStatusHandler merchantOperatingCityId fulfillmentHandler Payment.FRFSPassPurchase paymentOrder orderStatusCall
          logInfo $ "Reconciled orphaned payment " <> payment.id.getId <> " via payment order status check"
  case result of
    Left err ->
      logError $ "Failed to process orphaned payment " <> payment.id.getId <> ": " <> show err
    Right _ -> pure ()
