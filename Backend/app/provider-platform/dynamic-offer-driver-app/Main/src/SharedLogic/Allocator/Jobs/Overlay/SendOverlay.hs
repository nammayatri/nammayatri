{-# OPTIONS_GHC -Wno-type-defaults #-}

module SharedLogic.Allocator.Jobs.Overlay.SendOverlay where

import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.Overlay as DOverlay
import Domain.Types.Merchant.TransporterConfig
import qualified Domain.Types.Person as DP
import EulerHS.Prelude hiding (id)
import Kernel.External.Types
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator
import qualified SharedLogic.DriverFee as SLDriverFee
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
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
      overlayKey = jobData.overlayKey
      udf1 = jobData.udf1
      rescheduleInterval = jobData.rescheduleInterval
  -- transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)

  case jobData.condition of
    DOverlay.PaymentPendingGreaterThan limit -> do
      driverIds <- getBatchedDriverIds merchantId
      logInfo $ show driverIds
      driverIdsLength <- getPaymentPendingDriverIdsLength merchantId
      if driverIdsLength > 0
        then do
          mapM_ (sendPaymentPendingOverlay (fromIntegral limit) overlayKey udf1) driverIds
          ReSchedule . addUTCTime 180 <$> getCurrentTime -- 3 minutes
        else do
          unless (null driverIds) $ mapM_ (sendPaymentPendingOverlay (fromIntegral limit) overlayKey udf1) driverIds
          case rescheduleInterval of
            Just interval -> do
              ReSchedule . addUTCTime (fromIntegral interval) <$> getCurrentTime
            Nothing -> return Complete
    -- driversToNofify <- B.runInReplica $ findAllNotNotifiedWithDues merchantId transporterConfig.minOverlayGap 50
    -- driverFees <- findPendingPaymentByDrivers (driversToNofify <&> (.driverId))
    -- if null driverFees
    --     then return Complete
    --     else do
    --         groupBy (\a b -> )
    DOverlay.InactiveAutopay -> do
      return Complete
  where
    sendPaymentPendingOverlay limit overlayKey udf1 driverId = do
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      pendingDriverFees <- QDF.findAllPendingAndDueDriverFeeByDriverId driverId
      let manualDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf (fromIntegral dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) $ filter (\due -> due.status == DDF.PAYMENT_OVERDUE) pendingDriverFees
      when (manualDues > limit) $ sendOverlay driver overlayKey udf1 0

getRescheduledTime :: (MonadTime m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateNotificationRescheduleInterval <$> getCurrentTime

sendOverlay :: (CacheFlow m r, EsqDBFlow m r) => DP.Person -> Text -> Maybe Text -> Int -> m ()
sendOverlay driver overlayKey udf1 _ = do
  mOverlay <- CMP.findByMerchantIdPNKeyLangaugeUdf driver.merchantId overlayKey (fromMaybe ENGLISH driver.language) udf1
  whenJust mOverlay $ \overlay -> do
    -- let description = T.replace (templateText "saveUpto") (show saveUpto) <$> overlay.description
    TN.sendOverlay driver.merchantId driver.id driver.deviceToken overlay.title overlay.description overlay.imageUrl overlay.okButtonText overlay.cancelButtonText overlay.actions overlay.link overlay.endPoint overlay.method overlay.reqBody

getPaymentPendingDriverIdsLength :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m Integer
getPaymentPendingDriverIdsLength = Hedis.lLen . makePaymentPendingDriverIdsKey

getFirstNPaymentPendingDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Integer -> m [Id DP.Person]
getFirstNPaymentPendingDriverIds merchantId num = Hedis.lRange (makePaymentPendingDriverIdsKey merchantId) 0 (num -1)

deleteNPaymentPendingDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> Integer -> m ()
deleteNPaymentPendingDriverIds merchantId num = Hedis.lTrim (makePaymentPendingDriverIdsKey merchantId) num (-1)

addPaymentPendingDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> NonEmpty (Id DP.Person) -> m ()
addPaymentPendingDriverIds merchantId = Hedis.rPush (makePaymentPendingDriverIdsKey merchantId)

makePaymentPendingDriverIdsKey :: Id DM.Merchant -> Text
makePaymentPendingDriverIdsKey merchantId = "SendOverlayScheduler:PaymentPendingDriverIds:merchantId-" <> merchantId.getId

getBatchedDriverIds :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m [Id DP.Person]
getBatchedDriverIds merchantId = do
  driverIdsLength <- getPaymentPendingDriverIdsLength merchantId
  when (driverIdsLength < 1) do
    drivers <- QDI.fetchAllDriversWithPaymentPending merchantId
    whenJust (nonEmpty (drivers <&> (.driverId))) $ addPaymentPendingDriverIds merchantId
  batchedDriverIds <- getFirstNPaymentPendingDriverIds merchantId 50
  deleteNPaymentPendingDriverIds merchantId 50
  return batchedDriverIds
