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
import Domain.Types.MerchantConfig
import Domain.Types.Person
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id (cast)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, Log (withLogTag), MonadTime (getCurrentTime), addUTCTime, fromMaybeM, getLocalCurrentTime, logError, logInfo, throwError)
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.DriverFee
import Storage.CachedQueries.DriverInformation (updatePendingPayment, updateSubscription)
import qualified Storage.CachedQueries.Merchant.MerchantConfig as SCT
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Storage.Queries.DriverFee
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify

sendPaymentReminderToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Job 'SendPaymentReminderToDriver ->
  m ExecutionResult
sendPaymentReminderToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
      endTime = jobData.endTime
  now <- getLocalCurrentTime jobData.timeDiff
  feeZipDriver <- calcDriverFeeAttr ONGOING startTime endTime
  when (null feeZipDriver) $ logInfo "No ongoing payment found."
  for_ feeZipDriver $ \(driverFee, mbDriver) -> do
    case mbDriver of
      Nothing -> do
        logInfo "Driver Not found. This should not be possible."
        throwError (InternalError "Driver Not Found") -- Unreachable
      Just driver -> do
        overdueFeeNotif <- B.runInReplica $ findOldestFeeByStatus (cast driver.id) PAYMENT_OVERDUE
        let paymentTitle = "Bill generated"
            paymentMessage = "You have taken " <> show (driverFee.numRides + maybe 0 (.numRides) overdueFeeNotif) <> " ride(s) since the last payment. Complete payment now to get trips seamlessly"
        (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_PENDING paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for payment reminder to driver id " <> driver.id.getId <> " failed. Error: " <> show e
  forM_ feeZipDriver $ \(driverFee, mbPerson) -> do
    whenJust mbPerson $ \person -> do
      Redis.whenWithLockRedis (paymentProcessingLockKey driverFee.driverId.getId) 60 $ do
        overdueFee <- B.runInReplica $ findOldestFeeByStatus (cast person.id) PAYMENT_OVERDUE
        case overdueFee of
          Nothing -> do
            -- Esq.runTransaction $ updateStatus PAYMENT_PENDING driverFee.id now
            _ <- updateStatus PAYMENT_PENDING driverFee.id now
            updatePendingPayment True (cast person.id)
          Just oDFee -> do
            mergeDriverFee oDFee driverFee now
  case listToMaybe feeZipDriver of
    Nothing -> return Complete
    Just (driverFee, _) -> do
      driver <- B.runInReplica $ QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      -- driver <- QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      transporterConfig <- SCT.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
      ReSchedule <$> getRescheduledTime transporterConfig

unsubscribeDriverForPaymentOverdue ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Job 'UnsubscribeDriverForPaymentOverdue ->
  m ExecutionResult
unsubscribeDriverForPaymentOverdue Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
  now <- getLocalCurrentTime jobData.timeDiff
  feeZipDriver <- calcDriverFeeAttr PAYMENT_PENDING startTime now
  when (null feeZipDriver) $ logInfo "No pending payment found."
  for_ feeZipDriver $ \(driverFee, mbDriver) -> do
    case mbDriver of
      Nothing -> do
        logInfo "Driver Not found. This should not be possible."
        throwError (InternalError "Driver Not Found") -- Unreachable
      Just driver -> do
        let paymentTitle = "Bill generated"
            paymentMessage = "You have taken " <> show driverFee.numRides <> " ride(s) since the last payment. Complete payment now to get trips seamlessly"
        (Notify.sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.PAYMENT_OVERDUE paymentTitle paymentMessage driver.id driver.deviceToken) `C.catchAll` \e -> C.mask_ $ logError $ "FCM for removing subsciption of driver id " <> driver.id.getId <> " failed. Error: " <> show e
  forM_ feeZipDriver $ \(driverFee, mbPerson) -> do
    Redis.whenWithLockRedis (paymentProcessingLockKey driverFee.driverId.getId) 60 $ do
      -- Esq.runTransaction $ do
      _ <- updateStatus PAYMENT_OVERDUE driverFee.id now
      whenJust mbPerson $ \person -> do
        QDFS.updateStatus (cast person.id) DDFS.PAYMENT_OVERDUE
      whenJust mbPerson $ \person -> updateSubscription False (cast person.id) -- fix later: take tabular updates inside transaction
  return Complete

calcDriverFeeAttr :: (EsqDBFlow m r, Esq.EsqDBReplicaFlow m r) => DriverFeeStatus -> UTCTime -> UTCTime -> m [(DriverFee, Maybe Person)]
calcDriverFeeAttr driverFeeStatus startTime endTime = do
  driverFees <- findFeesInRangeWithStatus startTime endTime driverFeeStatus
  let relevantDriverIds = (.driverId) <$> driverFees
  relevantDrivers <- mapM (B.runInReplica . QPerson.findById) (cast <$> relevantDriverIds)
  -- relevantDrivers <- mapM QPerson.findById (cast <$> relevantDriverIds)
  return $ zip driverFees relevantDrivers

getRescheduledTime :: MonadTime m => MerchantConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.driverPaymentReminderInterval <$> getCurrentTime
