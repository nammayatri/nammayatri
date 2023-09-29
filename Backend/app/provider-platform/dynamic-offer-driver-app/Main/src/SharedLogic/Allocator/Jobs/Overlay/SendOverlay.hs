module SharedLogic.Allocator.Jobs.Overlay.SendOverlay where

import qualified Domain.Types.Merchant.Overlay as DOverlay
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Tools.Notifications as TN

sendOverlayToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    ServiceFlow m r,
    Esq.Transactionable m,
    EncFlow m r,
    HasShortDurationRetryCfg r c
  ) =>
  Job 'SendOverlay ->
  m ExecutionResult
sendOverlayToDriver (Job {id, jobInfo}) = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
  _ <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)

  case jobData.condition of
    DOverlay.PaymentPendingGreaterThan _ -> do
      return Complete
    -- driversToNofify <- B.runInReplica $ findAllNotNotifiedWithDues merchantId transporterConfig.minOverlayGap 50
    -- driverFees <- findPendingPaymentByDrivers (driversToNofify <&> (.driverId))
    -- if null driverFees
    --     then return Complete
    --     else do
    --         groupBy (\a b -> )
    DOverlay.InactiveAutopay -> do
      return Complete

-- else ReSchedule <$> getRescheduledTime transporterConfig

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

sendOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Text -> Maybe Text -> Int -> m ()
sendOverlay driver overlayKey udf1 _ = do
  mOverlay <- CMP.findByMerchantIdPNKeyLangaugeUdf driver.merchantId overlayKey (fromMaybe ENGLISH driver.language) udf1
  whenJust mOverlay $ \overlay -> do
    -- let description = T.replace (templateText "saveUpto") (show saveUpto) <$> overlay.description
    TN.sendOverlay driver.merchantId driver.id driver.deviceToken overlay.title overlay.description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody
