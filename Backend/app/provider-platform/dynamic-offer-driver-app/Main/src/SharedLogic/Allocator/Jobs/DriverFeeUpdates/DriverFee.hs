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
    calculateDriverFeeForDrivers,
  )
where

import qualified Control.Monad.Catch as C
import Data.Fixed (mod')
import qualified Data.Map as M
import Data.Ord
import Domain.Action.UI.Ride.EndRide.Internal (getDriverFeeCalcJobFlagKey, getPlan, mkDriverFeeCalcJobFlagKey)
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverFee
import Domain.Types.Merchant
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import Domain.Types.Plan (PaymentMode (AUTOPAY, MANUAL), Plan (..), PlanBaseAmount (..))
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast, getShortId)
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, GuidLike (generateGUID), HasShortDurationRetryCfg, HighPrecMoney (..), Log (withLogTag), MonadFlow, MonadGuid, MonadTime (getCurrentTime), addUTCTime, diffUTCTime, fromMaybeM, generateShortId, getLocalCurrentTime, logError, logInfo, secondsToNominalDiffTime, throwError, withShortRetry)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Storage.Queries.DriverFee as QDF
import Storage.Queries.DriverInformation (updatePendingPayment, updateSubscription)
import Storage.Queries.DriverPlan
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify
import qualified Tools.Payment as Payment
import qualified Tools.Payment as TPayment

sendPaymentReminderToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'SendPaymentReminderToDriver ->
  m ExecutionResult
sendPaymentReminderToDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
      endTime = jobData.endTime
      merchantId = jobData.merchantId
  now <- getLocalCurrentTime jobData.timeDiff
  feeZipDriver <- calcDriverFeeAttr merchantId ONGOING startTime endTime
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
        updateStatus PAYMENT_PENDING now driverFee.id
        updatePendingPayment True (cast person.id)
  case listToMaybe feeZipDriver of
    Nothing -> return Complete
    Just (driverFee, _) -> do
      driver <- B.runInReplica $ QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      -- driver <- QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      transporterConfig <- SCT.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
      ReSchedule <$> getRescheduledTime transporterConfig.driverPaymentReminderInterval

cacheDriverPlan :: (CacheFlow m r) => Id Driver -> UTCTime -> Plan -> m ()
cacheDriverPlan driverId time driverPlan = Hedis.setExp (makeDriverPlanKey driverId time) driverPlan 86399 -- Hedis.withCrossAppRedis $

makeDriverPlanKey :: Id Driver -> UTCTime -> Text
makeDriverPlanKey id day = "driver-offer:CachedQueries:DriverPlan:Date" <> show day <> "DriverId-" <> id.getId

getDriverPlanCache :: CacheFlow m r => UTCTime -> Id Driver -> m (Maybe Plan)
getDriverPlanCache time driverId = Hedis.get (makeDriverPlanKey driverId time)

calculateDriverFeeForDrivers ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Job 'CalculateDriverFees ->
  m ExecutionResult
calculateDriverFeeForDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  -- handle 1st time
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      startTime = jobData.startTime
      endTime = jobData.endTime
      applyOfferCall = TPayment.offerApply merchantId
  firstJobOfWindow <- getDriverFeeCalcJobFlagKey startTime endTime merchantId
  when (firstJobOfWindow == Just True) $ do
    driverFees <- findFeesInRangeWithStatus (Just merchantId) startTime endTime ONGOING Nothing
    for_ driverFees $ \driverFee -> do
      mbDriverPlan <- findByDriverId (cast driverFee.driverId)
      plan <- getPlan mbDriverPlan merchantId
      cacheDriverPlan driverFee.driverId endTime plan
    setDriverFeeBillNumberKey merchantId 1 36000 -- check here --
    Hedis.del (mkDriverFeeCalcJobFlagKey startTime endTime merchantId)
  -- Schedule notif job

  now <- getCurrentTime
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFees <- findFeesInRangeWithStatus (Just merchantId) startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize

  for_ driverFees $ \driverFee -> do
    plan <- getDriverPlanCache endTime driverFee.driverId >>= fromMaybeM (InternalError ("No plan found for driver" <> driverFee.driverId.getId))
    driverPlan <- findByDriverId (cast driverFee.driverId) >>= fromMaybeM (InternalError ("No driver plan found for driver" <> driverFee.driverId.getId))
    let (planBaseFrequcency, baseAmount) = getFreqAndBaseAmountcase plan.planBaseAmount
        dutyDate = driverFee.createdAt
        due = fromIntegral driverFee.govtCharges + fromIntegral driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst
        mandateSetupDate = fromMaybe now (driverPlan.mandateSetupDate)

    driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)

    (feeWithoutDiscount, totalFee, offerId, offerTitle) <- case planBaseFrequcency of
      "PER_RIDE" -> do
        let numRides = driverFee.numRides - plan.freeRideCount
            feeWithoutDiscount = min plan.maxAmount (baseAmount * HighPrecMoney (toRational numRides))
        getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount mandateSetupDate driverFee
      "DAILY" -> do
        let numRides = driverFee.numRides - plan.freeRideCount
            feeWithoutDiscount = if numRides > 0 then baseAmount else 0
        getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount mandateSetupDate driverFee
      _ -> return (0, 0, Nothing, Nothing) -- TODO: handle WEEKLY and MONTHLY later
    let offerAndPlanTitle = Just plan.id.getId <> Just "-*$*-" <> offerTitle ---- this we will send in payment history ----
    updateOfferAndPlanDetails offerId offerAndPlanTitle driverFee.id now
    offerTxnId <- getShortId <$> generateShortId
    let offerApplied = catMaybes [offerId]
        offerApplyRequest' = mkApplyOfferRequest offerTxnId offerApplied due driverPlan dutyDate mandateSetupDate
    maybe (pure ()) (\offerRequest -> do void $ try @_ @SomeException $ withShortRetry (applyOfferCall offerRequest)) (Just offerApplyRequest')
    ---- here we need the status to notification scheduled ----
    unless (totalFee == 0) $ do
      driverFeeSplitter plan feeWithoutDiscount totalFee driverFee now
      updatePendingPayment True (cast driverFee.driverId)

    -- blocking
    dueInvoices <- QDF.findAllPendingAndDueDriverFeeByDriverId (cast driverFee.driverId) -- Problem with lazy evaluation?
    let driverFeeIds = map (.id) dueInvoices
    when (due + totalFee >= plan.maxCreditLimit) $ do
      updateStatus PAYMENT_OVERDUE now driverFee.id
      updateFeeType RECURRING_INVOICE now `mapM_` driverFeeIds
      updateSubscription False (cast driverFee.driverId)
      QDFS.updateStatus (cast driverFee.driverId) DDFS.PAYMENT_OVERDUE -- only updating when blocked. Is this being used?
    unless (due + totalFee >= plan.maxCreditLimit) $ do
      QDF.updateAutopayPayementStageById (Just NOTIFICATION_SCHEDULED) driverFee.id

    updateSerialOrderForInvoicesInWindow driverFee.id merchantId startTime endTime

  case listToMaybe driverFees of
    Nothing -> do
      Hedis.del (mkDriverFeeBillNumberKey merchantId)
      maxShards <- asks (.maxShards)
      scheduleJobs transporterConfig startTime endTime merchantId maxShards now
      return Complete
    _ -> case transporterConfig.driverFeeCalculatorBatchGap of
      Nothing -> throwError $ InternalError "No batch gap defined for driver fee calculator job"
      Just gap -> ReSchedule <$> getRescheduledTime gap
  where
    mkApplyOfferRequest offerTxnUUID appliedOfferIds due driverPlan' dutyDate registrationDate =
      PaymentInterface.OfferApplyReq
        { txnId = offerTxnUUID,
          offers = appliedOfferIds,
          customerId = driverPlan'.driverId.getId,
          amount = due,
          currency = PaymentInterface.INR,
          planId = driverPlan'.planId.getId,
          registrationDate,
          dutyDate = dutyDate,
          paymentMode = show $ driverPlan'.planType
        }

buildRestFees :: MonadGuid m => DriverFeeStatus -> FeeType -> DriverFee -> m DriverFee
buildRestFees status_ feeType_ DriverFee {..} = do
  id_ <- generateGUID
  return
    DriverFee
      { id = id_,
        status = status_,
        feeType = feeType_,
        ..
      }

calculatePlatformFeeAttr :: HighPrecMoney -> Plan -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
calculatePlatformFeeAttr totalFee plan = do
  let platformFee = totalFee / HighPrecMoney (toRational $ 1 + plan.cgstPercentage + plan.sgstPercentage) -- this should be changed to HighPrecMoney
      cgst = HighPrecMoney (toRational plan.cgstPercentage) * platformFee
      sgst = HighPrecMoney (toRational plan.sgstPercentage) * platformFee
  (platformFee, cgst, sgst)

makeOfferReq :: HighPrecMoney -> Person -> Plan -> UTCTime -> UTCTime -> Payment.OfferListReq
makeOfferReq totalFee driver plan dutyDate registrationDate = do
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = totalFee, currency = Payment.INR} -- add UDFs
      customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
  Payment.OfferListReq
    { order = offerOrder,
      customer = Just customerReq,
      planId = plan.id.getId,
      registrationDate,
      paymentMode = show plan.paymentMode,
      dutyDate
    }

getFinalOrderAmount :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => HighPrecMoney -> Id Merchant -> TransporterConfig -> Person -> Plan -> HighPrecMoney -> UTCTime -> DriverFee -> m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount registrationDate driverFee = do
  now <- getCurrentTime
  let dutyDate = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) driverFee.createdAt
      registrationDateLocal = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) registrationDate
  if feeWithoutDiscount == 0
    then do
      updateStatus CLEARED now driverFee.id
      return (0, 0, Nothing, Nothing)
    else do
      offers <- Payment.offerList merchantId (makeOfferReq feeWithoutDiscount driver plan dutyDate registrationDateLocal) -- handle UDFs
      (finalOrderAmount, offerId, offerTitle) <-
        if null offers.offerResp
          then pure (baseAmount, Nothing, Nothing)
          else do
            let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers.offerResp
            pure (bestOffer.finalOrderAmount, Just bestOffer.offerId, bestOffer.offerDescription.title)
      let (platformFee, cgst, sgst) = calculatePlatformFeeAttr finalOrderAmount plan -- this should be HighPrecMoney
      updateFee driverFee.id Nothing 0 (round platformFee) cgst sgst now False -- add split logic before update
      return (feeWithoutDiscount, finalOrderAmount, offerId, offerTitle)

splitPlatformFee :: HighPrecMoney -> HighPrecMoney -> Plan -> DriverFee -> [DriverFee]
splitPlatformFee feeWithoutDiscount_ totalFee plan DriverFee {..} =
  let numEntities = totalFee / plan.maxAmount
      remainingFee = totalFee `mod'` plan.maxAmount
      entityList = replicate (floor numEntities) plan.maxAmount ++ [remainingFee | remainingFee > 0]
   in map
        ( \fee -> do
            let (platformFee_, cgst, sgst) = calculatePlatformFeeAttr fee plan
            DriverFee
              { platformFee = PlatformFee {fee = round platformFee_, ..},
                feeType = feeType,
                feeWithoutDiscount = Just feeWithoutDiscount_, -- same for all splitted ones, not remaining fee
                ..
              }
        )
        -- govt_charges, num_rides, total_earnings are same for all these
        entityList

getFreqAndBaseAmountcase :: PlanBaseAmount -> (Text, HighPrecMoney)
getFreqAndBaseAmountcase planBaseAmount = case planBaseAmount of
  PERRIDE_BASE amount -> ("PER_RIDE" :: Text, amount)
  DAILY_BASE amount -> ("DAILY" :: Text, amount)
  WEEKLY_BASE amount -> ("WEEKLY" :: Text, amount)
  MONTHLY_BASE amount -> ("MONTHLY" :: Text, amount)

driverFeeSplitter :: (MonadFlow m) => Plan -> HighPrecMoney -> HighPrecMoney -> DriverFee -> UTCTime -> m ()
driverFeeSplitter plan feeWithoutDiscount totalFee driverFee now = do
  let splittedFees = splitPlatformFee feeWithoutDiscount totalFee plan driverFee
  case plan.paymentMode of
    MANUAL -> do
      case splittedFees of
        [] -> throwError (InternalError "No driver fee entity with non zero total fee")
        (firstFee : restFees) -> do
          updateStatus PAYMENT_OVERDUE now firstFee.id
          updRestFees <- mapM (buildRestFees PAYMENT_OVERDUE RECURRING_INVOICE) restFees
          createMany updRestFees
    AUTOPAY -> do
      case splittedFees of
        [] -> throwError (InternalError "No driver fee entity with non zero total fee")
        (firstFee : restFees) -> do
          updateStatus PAYMENT_PENDING now firstFee.id
          updRestFees <- mapM (buildRestFees PAYMENT_PENDING RECURRING_EXECUTION_INVOICE) restFees
          createMany updRestFees

unsubscribeDriverForPaymentOverdue ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'UnsubscribeDriverForPaymentOverdue ->
  m ExecutionResult
unsubscribeDriverForPaymentOverdue Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      startTime = jobData.startTime
      merchantId = jobData.merchantId
  now <- getLocalCurrentTime jobData.timeDiff
  feeZipDriver <- calcDriverFeeAttr merchantId PAYMENT_PENDING startTime now
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
      _ <- updateStatus PAYMENT_OVERDUE now driverFee.id
      whenJust mbPerson $ \person -> do
        QDFS.updateStatus (cast person.id) DDFS.PAYMENT_OVERDUE
      whenJust mbPerson $ \person -> updateSubscription False (cast person.id) -- fix later: take tabular updates inside transaction
  return Complete

calcDriverFeeAttr :: (MonadFlow m) => Maybe (Id Merchant) -> DriverFeeStatus -> UTCTime -> UTCTime -> m [(DriverFee, Maybe Person)]
calcDriverFeeAttr merchantId driverFeeStatus startTime endTime = do
  driverFees <- findFeesInRangeWithStatus merchantId startTime endTime driverFeeStatus Nothing
  let relevantDriverIds = (.driverId) <$> driverFees
  relevantDrivers <- mapM (B.runInReplica . QPerson.findById) (cast <$> relevantDriverIds)
  -- relevantDrivers <- mapM QPerson.findById (cast <$> relevantDriverIds)
  return $ zip driverFees relevantDrivers

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

mkDriverFeeBillNumberKey :: Id Merchant -> Text
mkDriverFeeBillNumberKey merchantId = "DriverFeeCalulation:BillNumber:Counter" <> merchantId.getId

getDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> m (Maybe Int)
getDriverFeeBillNumberKey merchantId = Hedis.get (mkDriverFeeBillNumberKey merchantId)

setDriverFeeBillNumberKey :: CacheFlow m r => Id Merchant -> Int -> NominalDiffTime -> m ()
setDriverFeeBillNumberKey merchantId count expTime = Hedis.setExp (mkDriverFeeBillNumberKey merchantId) count (round expTime)

updateSerialOrderForInvoicesInWindow :: (MonadFlow m, CacheFlow m r) => Id DriverFee -> Id Merchant -> UTCTime -> UTCTime -> m ()
updateSerialOrderForInvoicesInWindow driverFeeId merchantId startTime endTime = do
  Hedis.whenWithLockRedis (billNumberGenerationLockKey driverFeeId.getId) 60 $ do
    counter <- getDriverFeeBillNumberKey merchantId
    when (isNothing counter) $ do
      count <- listToMaybe <$> QDF.findMaxBillNumberInRange merchantId startTime endTime
      void $ Hedis.incrby (mkDriverFeeBillNumberKey merchantId) (maybe 0 toInteger (count >>= (.billNumber)))
    billNumber' <- Hedis.incr (mkDriverFeeBillNumberKey merchantId)
    QDF.updateBillNumberById (Just (fromInteger billNumber')) driverFeeId

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool)) => TransporterConfig -> UTCTime -> UTCTime -> Id Merchant -> Int -> UTCTime -> m ()
scheduleJobs transporterConfig startTime endTime merchantId maxShards now = do
  let dfNotificationTime = transporterConfig.driverAutoPayNotificationTime
  let dfCalculationJobTs = diffUTCTime (addUTCTime dfNotificationTime endTime) now
  createJobIn @_ @'SendPDNNotificationToDriver dfCalculationJobTs maxShards $
    SendPDNNotificationToDriverJobData
      { merchantId = merchantId,
        startTime = startTime,
        endTime = endTime
      }
