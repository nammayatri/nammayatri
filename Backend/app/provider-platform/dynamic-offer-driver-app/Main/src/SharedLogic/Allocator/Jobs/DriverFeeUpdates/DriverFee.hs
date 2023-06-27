{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.DriverFeeUpdates.DriverFee
  ( sendPaymentReminderToDriver,
    unsubscribeDriverForPaymentOverdue,
  )
where

import qualified Control.Monad.Catch as C
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverFee
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import qualified EulerHS.Language as L
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id (cast)
import Kernel.Utils.Common (EsqDBFlow, Log (withLogTag), MonadFlow, MonadTime (getCurrentTime), Seconds, addUTCTime, fromMaybeM, logError, logInfo, secondsToNominalDiffTime, throwError)
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import Storage.CachedQueries.DriverInformation (updatePendingPayment, updateSubscription)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Storage.Queries.DriverFee
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify

sendPaymentReminderToDriver ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Job 'SendPaymentReminderToDriver ->
  m ExecutionResult
sendPaymentReminderToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
      endTime = jobData.endTime
      now = jobData.now
  feeZipDriver <- calcDriverFeeAttr ONGOING startTime endTime
  when (null feeZipDriver) $ logInfo "No ongoing payment found."
  for_ feeZipDriver $ \(driverFee, mbDriver) -> do
    case mbDriver of
      Nothing -> do
        logInfo "Driver Not found. This should not be possible."
        throwError (InternalError "Driver Not Found") -- Unreachable
      Just driver -> do
        let pendingAmount = driverFee.govtCharges + driverFee.platformFee.fee + round driverFee.platformFee.cgst + round driverFee.platformFee.sgst
            paymentTitle = "Payment Pending"
            paymentMessage = "Payment of " <> show pendingAmount.getMoney <> " is pending for " <> show driverFee.numRides <> " ride(s) with a deadline of " <> show driverFee.payBy <> ". Pay by " <> show driverFee.payBy <> " to avoid being blocked."
        (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_PENDING paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for payment reminder to driver id " <> driver.id.getId <> " failed. Error: " <> show e
  forM_ feeZipDriver $ \(driverFee, mbPerson) -> do
    Esq.runNoTransaction $ updateStatus PAYMENT_PENDING driverFee.id now
    whenJust mbPerson $ \person -> updatePendingPayment True (cast person.id)
  case listToMaybe feeZipDriver of
    Nothing -> return Complete
    Just (driverFee, _) -> do
      -- driver <- Esq.runInReplica $ QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      driver <- QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      transporterConfig <- SCT.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
      ReSchedule <$> getRescheduledTime transporterConfig

unsubscribeDriverForPaymentOverdue ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    L.MonadFlow m
  ) =>
  Job 'UnsubscribeDriverForPaymentOverdue ->
  m ExecutionResult
unsubscribeDriverForPaymentOverdue Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
  nowUtc <- getCurrentTime
  let now = getLocalTime nowUtc jobData.timeDiff
  feeZipDriver <- calcDriverFeeAttr PAYMENT_PENDING startTime now
  when (null feeZipDriver) $ logInfo "No pending payment found."
  for_ feeZipDriver $ \(driverFee, mbDriver) -> do
    case mbDriver of
      Nothing -> do
        logInfo "Driver Not found. This should not be possible."
        throwError (InternalError "Driver Not Found") -- Unreachable
      Just driver -> do
        let pendingAmount = driverFee.govtCharges + driverFee.platformFee.fee + round driverFee.platformFee.cgst + round driverFee.platformFee.sgst
        let paymentTitle = "Blocked - Payment Overdue"
            paymentMessage = "Sorry, you are blocked from taking rides as payment of " <> show pendingAmount.getMoney <> " is pending for " <> show driverFee.numRides <> " ride(s) with a deadline of " <> show driverFee.payBy <> ". Please pay the balance amount to get unblocked."
        (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_OVERDUE paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for removing subsciption of driver id " <> driver.id.getId <> " failed. Error: " <> show e
  forM_ feeZipDriver $ \(driverFee, mbPerson) -> do
    -- Esq.runTransaction $ do
    updateStatus PAYMENT_OVERDUE driverFee.id now
    whenJust mbPerson $ \person -> do
      QDFS.updateStatus (cast person.id) $ DDFS.PAYMENT_OVERDUE driverFee.id driverFee.govtCharges $ PlatformFee driverFee.platformFee.fee driverFee.platformFee.cgst driverFee.platformFee.sgst
    whenJust mbPerson $ \person -> updateSubscription False (cast person.id) -- fix later: take tabular updates inside transaction
  return Complete

calcDriverFeeAttr :: (MonadFlow m, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => DriverFeeStatus -> UTCTime -> UTCTime -> m [(DriverFee, Maybe Person)]
calcDriverFeeAttr driverFeeStatus startTime endTime = do
  driverFees <- findFeesInRangeWithStatus startTime endTime driverFeeStatus
  let relevantDriverIds = (.driverId) <$> driverFees
  relevantDrivers <- mapM (Esq.runInReplica . QPerson.findById) (cast <$> relevantDriverIds)
  return $ zip driverFees relevantDrivers

getRescheduledTime :: (MonadFlow m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = do
  addUTCTime tc.driverPaymentReminderInterval <$> getCurrentTime

getLocalTime :: UTCTime -> Seconds -> UTCTime
getLocalTime utcTime seconds = addUTCTime (secondsToNominalDiffTime seconds) utcTime
