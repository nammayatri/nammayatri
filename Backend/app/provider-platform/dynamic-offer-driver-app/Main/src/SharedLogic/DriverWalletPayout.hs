{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverWalletPayout
  ( driversWalletPayoutJob,
    driverWalletPayout,
    getJobId,
    resetDriversPayoutBatchNumber,
  )
where

import qualified Data.Text as T
import qualified Data.Time
import qualified Data.Time as Time
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverWallet as DW
import Domain.Types.Extra.Plan
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SubscriptionConfig as DSC
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Lib.Scheduler
import qualified Lib.Scheduler.JobStorageType.SchedulerType as QAllJ
import SharedLogic.Allocator
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverWallet as QDW
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

driversWalletPayoutJob ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    JobCreator r m,
    HasJobInfoMap r,
    HasKafkaProducer r
  ) =>
  Job 'DriversWalletPayout ->
  m ExecutionResult
driversWalletPayoutJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  merchant <- CQM.findById transporterConfig.merchantId >>= fromMaybeM (MerchantNotFound transporterConfig.merchantId.getId)
  unless (fromMaybe False merchant.enforceSufficientDriverBalance && fromMaybe False transporterConfig.enableWalletPayout) $
    throwError $ InvalidRequest "Payouts are disabled"
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing PREPAID_SUBSCRIPTION
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId "PREPAID_SUBSCRIPTION") -- Driver wallet is not required for postpaid
  merchantOperatingCity <-
    CQMOC.findById (Kernel.Types.Id.cast merchantOpCityId)
      >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  bn <- nextDriversPayoutBatchNumber merchantOpCityId
  let batchNumber = bn - 1
      limit = jobData.driversInBatch
      offset = batchNumber * limit
  when (limit <= 0) $ throwError (InternalError "Limit should be positive")
  driverInfos <- QDI.findAllByMerchantOperatingCityId limit offset merchantOpCityId
  logInfo $ "Running drivers payout job for batch: " <> show batchNumber <> "; drivers number: " <> show (length driverInfos)
  forM_ driverInfos $ \driverInfo -> do
    try (driverWalletPayout merchantOperatingCity transporterConfig subscriptionConfig driverInfo.driverId driverInfo.payoutVpa) >>= \case
      Left (err :: SomeException) -> logError $ "Drivers payout job failed for driverId: " <> driverInfo.driverId.getId <> "; batch: " <> show batchNumber <> "; err: " <> show err
      Right _ -> logInfo $ "Drivers payout job was completed successfully for driverId: " <> driverInfo.driverId.getId <> "; batch: " <> show batchNumber
    -- Rate limit for external api call
    threadDelayMilliSec jobData.eachDriverDelayMs
  if length driverInfos == limit
    then do
      timeAfterRun <- getCurrentTime
      logInfo $ "Drivers payout job was completed successfully for batch: " <> show batchNumber <> "; reschedule for new batch"
      pure $ ReSchedule $ addUTCTime (secondsToNominalDiffTime jobData.batchDelayS) timeAfterRun
    else do
      resetDriversPayoutBatchNumber merchantOpCityId
      logInfo $ "Drivers payout job was completed successfully for all batches: " <> show batchNumber
      now <- getCurrentTime
      let mbScheduledTime = do
            nextPayoutTime <- getNextPayoutTime jobData.startTime jobData.payoutPeriod
            if nextPayoutTime > addUTCTime 3600 now then Just nextPayoutTime else getNextPayoutTime now jobData.payoutPeriod
      case mbScheduledTime of
        Nothing -> logInfo "Single drivers payout job was finished. No new job scheduled"
        Just scheduledTime -> do
          jobs :: [AnyJob AllocatorJobType] <- QAllJ.getJobByTypeAndMerchantOperatingCityId (show DriversWalletPayout) merchantOpCityId
          let existingJobIds = filter (/= id) $ getJobId <$> jobs
          if null existingJobIds
            then do
              let updJobData = jobData{startTime = scheduledTime}
              logInfo $ "Create new drivers payout job by time: " <> show scheduledTime
              QAllJ.createJobByTime @_ @'DriversWalletPayout (Just transporterConfig.merchantId) (Just merchantOpCityId) scheduledTime updJobData
            else do
              logWarning $ "Couldn't create new drivers payout job. Found existing jobs: " <> show existingJobIds
      pure Complete

getNextPayoutTime :: UTCTime -> PayoutPeriod -> Maybe UTCTime
getNextPayoutTime _startTime SinglePayout = Nothing
getNextPayoutTime startTime (DailyPayout days) = Just $ addDaysToUTCTime startTime days
getNextPayoutTime startTime (WeeklyPayout weeks) = Just $ addWeeksToUTCTime startTime weeks
getNextPayoutTime startTime (MonthlyPayout months) = Just $ addMonthsToUTCTime startTime months

addDaysToUTCTime :: UTCTime -> Int -> UTCTime
addDaysToUTCTime startTime days = startTime {Time.utctDay = Time.addDays (fromIntegral days) (Time.utctDay startTime)}

addWeeksToUTCTime :: UTCTime -> Int -> UTCTime
addWeeksToUTCTime utcTime weeks = addDaysToUTCTime utcTime (weeks * 7)

-- Invalid day will be clipped to correct range: Apr 31 --> Apr 30
addMonthsToUTCTime :: UTCTime -> Int -> UTCTime
addMonthsToUTCTime utcTime months =
  let day = Time.utctDay utcTime
      (year, monthOfYear, dayOfMonth) = Time.toGregorian day
      totalMonths = monthOfYear + months
      newMonth = 1 + ((totalMonths - 1) `mod` 12)
      newYear = year + toInteger (totalMonths - 1) `div` 12
      newDay = Time.fromGregorian newYear newMonth dayOfMonth
   in utcTime {Time.utctDay = newDay}

getJobId :: forall t. AnyJob t -> Id AnyJob
getJobId (AnyJob Job {id}) = id

mkDriversPayoutBatchNumberKey :: Id DMOC.MerchantOperatingCity -> Text
mkDriversPayoutBatchNumberKey mocid = "DriversPayout:mocid:" <> mocid.getId

resetDriversPayoutBatchNumber :: (CacheFlow m r, Monad m, Log m, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m ()
resetDriversPayoutBatchNumber = Hedis.del . mkDriversPayoutBatchNumberKey

nextDriversPayoutBatchNumber :: (CacheFlow m r, Monad m, Log m, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m Int
nextDriversPayoutBatchNumber = fmap fromIntegral . (\a -> Hedis.incr a <* Hedis.expire a 86400) . mkDriversPayoutBatchNumberKey

driverWalletPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasKafkaProducer r
  ) =>
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  DSC.SubscriptionConfig ->
  Id DP.Person ->
  Maybe Text ->
  m APISuccess.APISuccess
driverWalletPayout merchantOperatingCity transporterConfig subscriptionConfig driverId mbVpa = do
  vpa <- fromMaybeM (InternalError $ "Payout vpa not present for " <> driverId.getId) mbVpa
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey driverId.getId) 10 10 $ do
    lastTransaction <- QDW.findLatestByDriverId driverId
    -- Should not use driverInfo.walletBalance, because driverInfo was fetched outside of lock
    let walletBalance = maybe 0 (.runningBalance) lastTransaction
        minPayoutAmount = fromMaybe 0 transporterConfig.minimumWalletPayoutAmount
    utcTimeNow <- getCurrentTime
    let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
        localTime = addUTCTime timeDiff utcTimeNow
        localDay = Data.Time.utctDay localTime
        startOfLocalDay = Data.Time.UTCTime localDay 0
        endOfLocalDay = Data.Time.UTCTime localDay 86399
        utcStartOfDay = addUTCTime (negate timeDiff) startOfLocalDay
        utcEndOfDay = addUTCTime (negate timeDiff) endOfLocalDay
    whenJust transporterConfig.maxWalletPayoutsPerDay $ \maxPayoutsPerDay -> do
      payoutsToday <- QDW.findAllByDriverIdRangeAndTransactionType driverId utcStartOfDay utcEndOfDay (Just DW.PAYOUT) (Just $ maxPayoutsPerDay + 1) Nothing
      when (length payoutsToday >= maxPayoutsPerDay) $ throwError $ InvalidRequest "Maximum payouts per day reached"
    payoutId <- generateGUID
    phoneNo <- mapM decrypt person.mobileNumber
    payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) person.clientSdkVersion person.merchantOperatingCityId
    let cutOffDate = Data.Time.addDays (negate (fromIntegral $ fromMaybe 0 transporterConfig.payoutCutOffDays)) localDay
        utcCutOffTime = addUTCTime (negate timeDiff) (Data.Time.UTCTime cutOffDate 0)
    transactionsAfterCutoff <- QDW.findAllByDriverIdRangeAndTransactionType driverId utcCutOffTime utcTimeNow Nothing Nothing Nothing
    let unsettledReceivables = sum $ mapMaybe (.merchantPayable) transactionsAfterCutoff
        payoutableBalance = walletBalance - unsettledReceivables
    when (payoutableBalance < minPayoutAmount) $ throwError $ InvalidRequest ("Minimum payout amount is " <> show minPayoutAmount)
    let createPayoutOrderReq = mkPayoutReq person vpa payoutId phoneNo payoutableBalance
        entityName = DPayment.DRIVER_WALLET_TRANSACTION
        createPayoutOrderCall = Payout.createPayoutOrder person.merchantId person.merchantOperatingCityId payoutServiceName (Just person.id.getId)
    logDebug $ "calling create payoutOrder with driverId: " <> driverId.getId <> " | amount: " <> show createPayoutOrderReq.amount <> " | orderId: " <> show payoutId
    when (createPayoutOrderReq.amount > 0.0) $ do
      (_, mbPayoutOrder) <- DPayment.createPayoutService (Kernel.Types.Id.cast person.merchantId) (Just $ Kernel.Types.Id.cast person.merchantOperatingCityId) (Kernel.Types.Id.cast driverId) Nothing (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
      whenJust mbPayoutOrder $ \payoutOrder -> do
        newId <- generateGUID
        let transaction =
              DW.DriverWallet
                { id = newId,
                  merchantId = Just person.merchantId,
                  merchantOperatingCityId = merchantOperatingCity.id,
                  driverId = driverId,
                  rideId = Nothing,
                  transactionType = DW.PAYOUT,
                  collectionAmount = Nothing,
                  gstDeduction = Nothing,
                  merchantPayable = Nothing,
                  driverPayable = Just (negate payoutableBalance),
                  runningBalance = unsettledReceivables,
                  payoutOrderId = Just payoutOrder.id,
                  payoutStatus = Just DW.INITIATED,
                  createdAt = utcTimeNow,
                  updatedAt = utcTimeNow
                }
        QDW.create transaction
        QDI.updateWalletBalance (Just unsettledReceivables) driverId
        let notificationTitle = "Payout Initiated"
            notificationMessage = "Your payout of " <> show payoutableBalance <> " has been initiated."
        Notify.sendNotificationToDriver person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED notificationTitle notificationMessage person person.deviceToken
  pure APISuccess.Success

mkPayoutReq :: DP.Person -> Text -> T.Text -> Maybe Text -> HighPrecMoney -> Juspay.CreatePayoutOrderReq
mkPayoutReq person vpa uid phoneNo amount =
  Juspay.CreatePayoutOrderReq
    { orderId = uid,
      amount = SPayment.roundToTwoDecimalPlaces amount,
      customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
      customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
      customerId = person.id.getId,
      orderType = "FULFILL_ONLY",
      remark = "Settlement for wallet",
      customerName = person.firstName,
      customerVpa = vpa,
      isDynamicWebhookRequired = False
    }
