module SharedLogic.Allocator.Jobs.Mandate.OrderAndNotificationStatusUpdate where

import Data.List (nubBy)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.UI.Payment as SharedPayment
import qualified Domain.Types.Invoice as INV
import Domain.Types.TransporterConfig
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (cast)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Notification as QNTF

notificationAndOrderStatusUpdate ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    ServiceFlow m r,
    Esq.Transactionable m,
    EncFlow m r,
    EventStreamFlow m r,
    HasShortDurationRetryCfg r c,
    SchedulerFlow r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'OrderAndNotificationStatusUpdate ->
  m ExecutionResult
notificationAndOrderStatusUpdate (Job {id, jobInfo}) = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      mbMerchantOpCityId = jobData.merchantOperatingCityId
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let batchSizeOfNotification = transporterConfig.updateNotificationStatusBatchSize
      batchSizeOfOrderStatus = transporterConfig.updateOrderStatusBatchSize
  allPendingNotification <- QNTF.findAllByStatusWithLimit [PaymentInterface.NOTIFICATION_CREATED, PaymentInterface.PENDING] merchantOpCityId batchSizeOfNotification
  QNTF.updateLastCheckedOn ((.id) <$> allPendingNotification)
  QNTF.updatePendingToFailed merchantOpCityId
  allPendingOrders <- nubBy ((==) `on` (.id)) <$> QINV.findAllByStatusWithLimit INV.ACTIVE_INVOICE merchantOpCityId batchSizeOfOrderStatus
  QINV.updateLastCheckedOn ((.id) <$> allPendingOrders)
  updateInvoicesPendingToFailedAfterRetry transporterConfig
  forM_ allPendingNotification $ \notification -> do
    fork ("notification status call for notification id : " <> notification.id.getId) $ do
      driverFee <- QDF.findById notification.driverFeeId >>= fromMaybeM (InternalError "Fee not found")
      void $ SharedPayment.pdnNotificationStatus (driverFee.driverId, merchantId, merchantOpCityId) notification.id
  forM_ allPendingOrders $ \invoice -> do
    fork ("order call for order id : " <> invoice.id.getId) $ do
      let driverId = invoice.driverId
      void $ SharedPayment.getStatus (driverId, jobData.merchantId, merchantOpCityId) (cast invoice.id)

  if null allPendingNotification && null allPendingOrders
    then do
      return Complete
    else do
      ReSchedule <$> getRescheduledTime transporterConfig

getRescheduledTime :: (MonadTime m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

updateInvoicesPendingToFailedAfterRetry :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
updateInvoicesPendingToFailedAfterRetry transporterConfig = do
  let timeCheckLimit = transporterConfig.orderAndNotificationStatusCheckTimeLimit
      opCityId = transporterConfig.merchantOperatingCityId
  activeExecutionInvoices <- QINV.findAllAutoPayInvoicesActiveOlderThanProvidedDuration timeCheckLimit opCityId
  QINV.updatePendingToFailed timeCheckLimit opCityId
  mapM_ QDF.updateAutoPayToManual (activeExecutionInvoices <&> (.driverFeeId))
