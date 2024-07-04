module SharedLogic.Allocator.Jobs.Mandate.OrderAndNotificationStatusUpdate where

import Data.List (nubBy)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.UI.Payment as SharedPayment
import qualified Domain.Action.UI.ReferralPayout as Payout
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Invoice as INV
import Domain.Types.TransporterConfig
import qualified Kernel.External.Payment.Interface.Types as PaymentInterface
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Types.Id (cast)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.Common as PD
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrdersExtra as PE
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.DailyStats as QDailyStats
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
    SchedulerFlow r
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
      batchSizeOfPayoutStatus = transporterConfig.updatePayoutStatusBatchSize
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

  daysToCheck <- getLastNDays transporterConfig
  dailyStatsForLastNDays <- QDailyStats.findAllByDatesInAndPayoutStatus daysToCheck DS.Processing
  let entityIds = map (Just . (.id)) dailyStatsForLastNDays
      entityName = PD.DRIVER_DAILY_STATS
  allPendingPayoutOrders <- QPayoutOrder.findAllByEntityNameAndEntityIds (Just batchSizeOfPayoutStatus) (Just 0) (Just entityName) entityIds
  PE.updateLastCheckedOn ((.orderId) <$> allPendingPayoutOrders)
  forM_ allPendingPayoutOrders $ \payoutOrder -> do
    fork ("payout orderStatus call for payout order id : " <> payoutOrder.orderId) $ do
      mbPersonId <-
        case payoutOrder.entityId of
          Just dStatsId -> pure $ (.driverId) <$> find (\ds -> ds.id == dStatsId) dailyStatsForLastNDays
          Nothing -> pure Nothing
      void $ Payout.getPayoutOrderStatus (mbPersonId, merchantId, merchantOpCityId) payoutOrder.entityId payoutOrder.orderId

  if null allPendingNotification && null allPendingOrders && null allPendingPayoutOrders
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

getLastNDays :: (MonadTime m) => TransporterConfig -> m [Day]
getLastNDays transporterConfig = do
  let lastNDays = transporterConfig.lastNdaysToCheckForPayoutOrderStatus
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let currentDay = utctDay localTime
      days = take lastNDays $ iterate (addDays (-1)) currentDay
  return days
