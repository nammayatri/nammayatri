{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CheckRefundStatus where

import qualified Domain.Types.Person as DP
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.Refunds as QRefunds
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics.BAPMetrics (HasBAPMetrics)
import qualified Tools.Payment as Payment
import qualified UrlShortner.Common as UrlShortner

checkRefundStatusJob ::
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
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  Job 'CheckRefundStatus ->
  m ExecutionResult
checkRefundStatusJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      refundId = Id jobData.refundId
      currentRetries = jobData.numberOfRetries
  refundEntry <- QRefunds.findById refundId >>= fromMaybeM (InvalidRequest $ "refund not found for id: " <> show refundId)
  paymentOrder <- QPaymentOrder.findByShortId refundEntry.orderId >>= fromMaybeM (InvalidRequest $ "payment order not found for refund: " <> show refundEntry.id.getId)
  person <- QP.findById (cast paymentOrder.personId) >>= fromMaybeM (PersonNotFound paymentOrder.personId.getId)
  retryStatus <- processRefundStatus refundEntry person paymentOrder
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  when retryStatus $ do
    let maxRetries = riderConfig.refundStatusUpdateRetries
    if currentRetries >= maxRetries
      then do
        logInfo $ "Maximum retries reached for refund " <> refundId.getId <> ". Stopping further checks."
      else do
        let nextSchedule = riderConfig.refundStatusUpdateInterval * (2 ^ currentRetries)
            newJobData =
              CheckRefundStatusJobData
                { refundId = refundId.getId,
                  numberOfRetries = currentRetries + 1
                }
        createJobIn @_ @'CheckRefundStatus (Just person.merchantId) (Just person.merchantOperatingCityId) nextSchedule (newJobData :: CheckRefundStatusJobData)
        logInfo $ "Scheduled next refund status check for " <> refundId.getId <> " in " <> show nextSchedule <> " (retry " <> show (currentRetries + 1) <> "/" <> show maxRetries <> ")"
  return Complete

processRefundStatus ::
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
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  DRefunds.Refunds ->
  DP.Person ->
  DPaymentOrder.PaymentOrder ->
  m Bool
processRefundStatus refundEntry person paymentOrder = do
  let nonTerminalStatuses = [Payment.REFUND_PENDING, Payment.MANUAL_REVIEW]
      paymentServiceType = fromMaybe Payment.FRFSMultiModalBooking paymentOrder.paymentServiceType
  if refundEntry.status `elem` nonTerminalStatuses
    then do
      let orderStatusCall = Payment.orderStatus person.merchantId person.merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion
          commonPersonId = Kernel.Types.Id.cast @DP.Person @DPayment.Person person.id
      paymentStatusResponse <- DPayment.orderStatusService commonPersonId paymentOrder.id orderStatusCall
      let matchingRefund = find (\refund -> refund.requestId == refundEntry.id.getId) paymentStatusResponse.refunds
      case matchingRefund of
        Just refund -> do
          let newStatus = refund.status
          when (newStatus /= refundEntry.status) $ do
            DPayment.updateRefundStatus refund
            logInfo $ "Updated refund status for " <> refundEntry.id.getId <> " to " <> show newStatus

          when (newStatus `notElem` nonTerminalStatuses) $ do
            void $ SPayment.orderStatusHandler paymentServiceType paymentOrder paymentStatusResponse
          return True
        Nothing -> return False
    else return False
