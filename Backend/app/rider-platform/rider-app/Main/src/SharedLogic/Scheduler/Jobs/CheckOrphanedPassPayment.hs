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
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPass as QPurchasedPass
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment
import qualified Tools.Metrics.BAPMetrics as Metrics
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

-- | Scheduled job that finds passes stuck in PENDING state for > 30 minutes
-- and reconciles them by checking payment status at Juspay.
--
-- For each orphaned pass:
--   - If payment succeeded at Juspay → activate the pass (via passOrderStatusHandler)
--   - If payment failed at Juspay → mark pass as Failed
--   - If payment not found → mark pass as Failed
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
  -- Find passes in PENDING state created more than 30 minutes ago
  let cutoffTime = addUTCTime (intToNominalDiffTime (-1800)) now
  orphanedPasses <- QPurchasedPass.findAllPendingOlderThan cutoffTime (Just 100)

  logInfo $ "Found " <> show (length orphanedPasses) <> " orphaned passes in PENDING state older than 30 minutes"

  mapM_ (processOrphanedPass merchantId' merchantOperatingCityId') orphanedPasses

  -- Self-schedule next run in 30 minutes
  let newJobData =
        CheckOrphanedPassPaymentJobData
          { merchantId = merchantId',
            merchantOperatingCityId = merchantOperatingCityId'
          }
  createJobIn @_ @'CheckOrphanedPassPayment (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 1800) newJobData
  logInfo "Scheduled next orphaned pass payment check job in 30 minutes"
  return Complete

processOrphanedPass ::
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
  DPurchasedPass.PurchasedPass ->
  m ()
processOrphanedPass merchantId merchantOperatingCityId purchasedPass = do
  result <-
    withTryCatch "checkOrphanedPassPayment:processOrphanedPass" $ do
      -- Find associated payment record
      payments <- QPurchasedPassPayment.findAllByPurchasedPassId purchasedPass.id
      case listToMaybe payments of
        Nothing -> do
          -- No payment record at all — mark pass as Failed
          logWarning $ "No payment record found for orphaned pass " <> purchasedPass.id.getId <> ", marking as Failed"
          QPurchasedPass.updateStatusById DPurchasedPass.Failed purchasedPass.id
        Just payment -> do
          -- Payment record exists — check status at payment gateway
          mbPaymentOrder <- QPaymentOrder.findById payment.orderId
          case mbPaymentOrder of
            Nothing -> do
              logWarning $ "No payment order found for orphaned pass " <> purchasedPass.id.getId <> " orderId " <> payment.orderId.getId <> ", marking as Failed"
              QPurchasedPass.updateStatusById DPurchasedPass.Failed purchasedPass.id
              QPurchasedPassPayment.updateStatusByOrderId DPurchasedPass.Failed payment.orderId
            Just paymentOrder -> do
              -- Use the existing payment order status handler to reconcile
              -- This will call passOrderStatusHandler which updates both pass and payment
              person <- QPerson.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
              let orderStatusCall = Payment.orderStatus merchantId merchantOperatingCityId Nothing Payment.FRFSPassPurchase (Just person.id.getId) person.clientSdkVersion paymentOrder.isMockPayment
                  fulfillmentHandler paymentStatusResp = do
                    status <- DPayment.getTransactionStatus paymentStatusResp
                    Pass.passOrderStatusHandler (cast merchantId) paymentOrder.id status
              void $ SPayment.orderStatusHandler merchantOperatingCityId fulfillmentHandler Payment.FRFSPassPurchase paymentOrder orderStatusCall
              logInfo $ "Reconciled orphaned pass " <> purchasedPass.id.getId <> " via payment order status check"
  case result of
    Left err ->
      logError $ "Failed to process orphaned pass " <> purchasedPass.id.getId <> ": " <> show err
    Right _ -> pure ()
