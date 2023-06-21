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
import Domain.Types.DriverFee
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common (EsqDBFlow, Log (withLogTag), MonadFlow, MonadTime (getCurrentTime), addUTCTime, fromMaybeM, logError, logInfo, throwError)
import Lib.Scheduler
import SharedLogic.Allocator
import Storage.CachedQueries.CacheConfig (HasCacheConfig)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.DriverFee
import Storage.Queries.DriverInformation (updateSubscription)
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
  (driverFees, driverFeeIds, feeZipDriver) <- calcDriverFeeAttr ONGOING startTime endTime
  if not $ null driverFees
    then do
      for_ feeZipDriver $ \(driverFee, mbDriver) -> do
        case mbDriver of
          Nothing -> do
            logInfo "Driver Not found. This should not be possible."
            throwError (InternalError "Driver Not Found") -- Unreachable
          Just driver -> do
            let paymentTitle = "Payment Pending"
                paymentMessage = "Payment of Rs. " <> show driverFee.totalAmount.getMoney <> " is pending. Pay by " <> show driverFee.payBy <> " to avoid being blocked."
            (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_PENDING paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for payment reminder to driver id " <> driver.id.getId <> " failed. Error: " <> show e
      Esq.runNoTransaction $ mapM_ (updateStatus PAYMENT_PENDING) driverFeeIds
      case listToMaybe driverFees of
        Nothing -> return Complete
        Just driverFee -> do
          driver <- Esq.runInReplica $ QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
          transporterConfig <- SCT.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
          ReSchedule <$> getRescheduledTime transporterConfig
    else do
      logInfo "No ongoing payment found."
      return Complete

unsubscribeDriverForPaymentOverdue ::
  ( MonadFlow m,
    HedisFlow m r,
    CoreMetrics m,
    HasCacheConfig r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Job 'UnsubscribeDriverForPaymentOverdue ->
  m ExecutionResult
unsubscribeDriverForPaymentOverdue Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
  now <- getCurrentTime
  (driverFees, _, feeZipDriver) <- calcDriverFeeAttr PAYMENT_PENDING startTime now
  if not $ null driverFees
    then do
      for_ feeZipDriver $ \(driverFee, mbDriver) -> do
        case mbDriver of
          Nothing -> do
            logInfo "Driver Not found. This should not be possible."
            throwError (InternalError "Driver Not Found") -- Unreachable
          Just driver -> do
            let paymentTitle = "Blocked - Payment Overdue"
                paymentMessage = "Sorry, you are blocked from taking rides as payment of Rs. " <> show driverFee.totalAmount.getMoney <> " is outstanding. Please pay the balance to get unblocked."
            (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_OVERDUE paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for removing subsciption of driver id " <> driver.id.getId <> " failed. Error: " <> show e
      forM_ feeZipDriver $ \(driverFee, mbPerson) -> Esq.runTransaction $ do
        updateStatus PAYMENT_OVERDUE driverFee.id
        whenJust mbPerson $ \person -> updateSubscription False (cast person.id)
    else do
      logInfo "No ongoing payment found."
  return Complete

calcDriverFeeAttr :: (MonadFlow m, EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => DriverFeeStatus -> UTCTime -> UTCTime -> m ([DriverFee], [Id DriverFee], [(DriverFee, Maybe Person)])
calcDriverFeeAttr driverFeeStatus startTime endTime = do
  driverFees <- findFeesInRangeWithStatus startTime endTime driverFeeStatus
  let driverFeeIds = (.id) <$> driverFees
      relevantDriverIds = (.driverId) <$> driverFees
  relevantDrivers <- mapM (Esq.runInReplica . QPerson.findById) (cast <$> relevantDriverIds)
  let feeZipDriver = zip driverFees relevantDrivers
  return (driverFees, driverFeeIds, feeZipDriver)

getRescheduledTime :: (MonadFlow m) => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.driverPaymentReminderInterval <$> getCurrentTime
