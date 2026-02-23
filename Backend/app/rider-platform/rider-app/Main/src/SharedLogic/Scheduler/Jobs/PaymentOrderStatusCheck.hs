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
  allRecentNonTerminalOrders <- QPaymentOrder.findAllNonTerminalOrders (addUTCTime (intToNominalDiffTime (-3600)) now)

  logInfo $ "Found " <> show (length allRecentNonTerminalOrders) <> " payment orders with status not CHARGED (or CHARGED with pending refunds) created within the last hour"

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
  void $ SPayment.orderStatusHandler fulfillmentHandler paymentServiceType paymentOrder orderStatusCall
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
