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
import Control.Monad.Extra (mapMaybeM)
import Data.Fixed (mod')
import qualified Data.Map as M
import Data.Ord
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Domain.Action.UI.Ride.EndRide.Internal (getDriverFeeBillNumberKey, getPlan, mkDriverFeeBillNumberKey)
import Domain.Types.DriverFee
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import Domain.Types.Merchant.Overlay (OverlayCondition (..))
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
import Kernel.Types.Id (Id (Id), cast, getShortId)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.DriverFee (roundToHalf)
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import Storage.Queries.DriverFee as QDF
import Storage.Queries.DriverInformation (updatePendingPayment, updateSubscription)
import Storage.Queries.DriverPlan
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QMD
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as Notify
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
        updateStatus PAYMENT_PENDING driverFee.id now
        updatePendingPayment True (cast person.id)
  case listToMaybe feeZipDriver of
    Nothing -> return Complete
    Just (driverFee, _) -> do
      driver <- B.runInReplica $ QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      -- driver <- QPerson.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
      transporterConfig <- SCT.findByMerchantId driver.merchantId >>= fromMaybeM (TransporterConfigNotFound driver.merchantId.getId)
      ReSchedule <$> getRescheduledTime transporterConfig.driverPaymentReminderInterval

calculateDriverFeeForDrivers ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
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
  now <- getCurrentTime
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFees <- findAllFeesInRangeWithStatus (Just merchantId) startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize
  let threshold = transporterConfig.driverFeeRetryThresholdConfig
  driverFeesToProccess <-
    mapMaybeM
      ( \driverFee -> do
          let count = driverFee.schedulerTryCount
              driverFeeId = driverFee.id
          if count > threshold
            then do
              QDF.updateAutoPayToManual driverFeeId
              return Nothing
            else do
              QDF.updateRetryCount (count + 1) now driverFeeId
              return (Just driverFee)
      )
      driverFees
  flip C.catchAll (\e -> C.mask_ $ logError $ "Driver fee scheduler for merchant id " <> merchantId.getId <> " failed. Error: " <> show e) $ do
    for_ driverFeesToProccess $ \driverFee -> do
      mbDriverPlan <- findByDriverId (cast driverFee.driverId)
      mbPlan <- getPlan mbDriverPlan merchantId
      case mbPlan of
        Nothing -> pure ()
        Just plan -> do
          let (planBaseFrequcency, baseAmount) = getFreqAndBaseAmountcase plan.planBaseAmount
              dutyDate = driverFee.createdAt
              (mandateSetupDate, mandateId) = case mbDriverPlan of
                Nothing -> (now, Nothing)
                Just driverPlan -> (fromMaybe now driverPlan.mandateSetupDate, driverPlan.mandateId)

          driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)

          (feeWithoutDiscount, totalFee, offerId, offerTitle) <- case planBaseFrequcency of
            "PER_RIDE" -> do
              let numRides = driverFee.numRides - plan.freeRideCount
                  feeWithoutDiscount = max 0 (min plan.maxAmount (baseAmount * HighPrecMoney (toRational numRides)))
              getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate driverFee
            "DAILY" -> do
              let numRides = driverFee.numRides - plan.freeRideCount
                  feeWithoutDiscount = if numRides > 0 then baseAmount else 0
              getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate driverFee
            _ -> return (0, 0, Nothing, Nothing) -- TODO: handle WEEKLY and MONTHLY later
          let offerAndPlanTitle = Just plan.name <> Just "-*@*-" <> offerTitle ---- this we will send in payment history ----
          updateOfferAndPlanDetails offerId offerAndPlanTitle driverFee.id now

          fork "Applying offer" $ do
            offerTxnId <- getShortId <$> generateShortId
            let offerApplied = catMaybes [offerId]
                offerApplyRequest' = mkApplyOfferRequest offerTxnId offerApplied feeWithoutDiscount plan driverFee.driverId dutyDate mandateSetupDate driverFee.numRides
            maybe (pure ()) (\offerRequest -> do void $ try @_ @SomeException $ withShortRetry (applyOfferCall offerRequest)) (Just offerApplyRequest')

          let paymentMode = maybe MANUAL (.planType) mbDriverPlan
          unless (totalFee == 0) $ do
            driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFee mandateId now
            updatePendingPayment True (cast driverFee.driverId)

          -- blocking
          dueDriverFees <- QDF.findAllPendingAndDueDriverFeeByDriverId (cast driverFee.driverId) -- Problem with lazy evaluation?
          let driverFeeIds = map (.id) dueDriverFees
              due = sum $ map (\fee -> roundToHalf $ fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst) dueDriverFees
          if roundToHalf (due + totalFee) >= plan.maxCreditLimit
            then do
              mapM_ updateAutoPayToManual driverFeeIds
              updateAutoPayToManual driverFee.id
              updateSubscription False (cast driverFee.driverId)
            else do
              unless (totalFee == 0) $ processDriverFee paymentMode driverFee

          updateSerialOrderForInvoicesInWindow driverFee.id merchantId startTime endTime

  case listToMaybe driverFees of
    Nothing -> do
      Hedis.del (mkDriverFeeBillNumberKey merchantId)
      maxShards <- asks (.maxShards)
      scheduleJobs transporterConfig startTime endTime merchantId maxShards
      return Complete
    _ -> ReSchedule <$> getRescheduledTime (fromMaybe 5 transporterConfig.driverFeeCalculatorBatchGap)
  where
    mkApplyOfferRequest offerTxnUUID appliedOfferIds due plan driverId dutyDate registrationDate numOfRides =
      PaymentInterface.OfferApplyReq
        { txnId = offerTxnUUID,
          offers = appliedOfferIds,
          customerId = driverId.getId,
          amount = due,
          currency = PaymentInterface.INR,
          planId = plan.id.getId,
          registrationDate,
          dutyDate = dutyDate,
          paymentMode = show $ plan.paymentMode,
          numOfRides
        }

processDriverFee :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => PaymentMode -> DriverFee -> m ()
processDriverFee paymentMode driverFee = do
  now <- getCurrentTime
  case paymentMode of
    MANUAL -> do
      updateAutoPayToManual driverFee.id
    AUTOPAY -> do
      updateStatus PAYMENT_PENDING driverFee.id now
      updateFeeType RECURRING_EXECUTION_INVOICE now driverFee.id
      invoice <- mkInvoiceAgainstDriverFee driverFee
      QINV.create invoice
      QDF.updateAutopayPaymentStageById (Just NOTIFICATION_SCHEDULED) driverFee.id

processRestFee :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => PaymentMode -> DriverFee -> m ()
processRestFee paymentMode DriverFee {..} = do
  id_ <- generateGUID
  let driverFee =
        DriverFee
          { id = id_,
            status = if paymentMode == MANUAL then PAYMENT_OVERDUE else PAYMENT_PENDING,
            feeType = if paymentMode == MANUAL then RECURRING_INVOICE else RECURRING_EXECUTION_INVOICE,
            ..
          }
  QDF.create driverFee
  processDriverFee paymentMode driverFee
  updateSerialOrderForInvoicesInWindow driverFee.id merchantId startTime endTime

calculatePlatformFeeAttr :: HighPrecMoney -> Plan -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
calculatePlatformFeeAttr totalFee plan = do
  let platformFee = totalFee / HighPrecMoney (toRational $ 1 + plan.cgstPercentage + plan.sgstPercentage) -- this should be changed to HighPrecMoney
      cgst = HighPrecMoney (toRational plan.cgstPercentage) * platformFee
      sgst = HighPrecMoney (toRational plan.sgstPercentage) * platformFee
  (platformFee, cgst, sgst)

makeOfferReq :: HighPrecMoney -> Person -> Plan -> UTCTime -> UTCTime -> Int -> Payment.OfferListReq
makeOfferReq totalFee driver plan dutyDate registrationDate numOfRides = do
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = totalFee, currency = Payment.INR} -- add UDFs
      customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
  Payment.OfferListReq
    { order = offerOrder,
      customer = Just customerReq,
      planId = plan.id.getId,
      registrationDate,
      paymentMode = show plan.paymentMode,
      dutyDate,
      numOfRides
    }

getFinalOrderAmount :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => HighPrecMoney -> Id Merchant -> TransporterConfig -> Person -> Plan -> UTCTime -> DriverFee -> m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan registrationDate driverFee = do
  now <- getCurrentTime
  let dutyDate = driverFee.createdAt
      registrationDateLocal = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) registrationDate
  if feeWithoutDiscount == 0
    then do
      updateCollectedPaymentStatus CLEARED Nothing now driverFee.id
      return (0, 0, Nothing, Nothing)
    else do
      offers <- SPayment.offerListCache merchantId (makeOfferReq feeWithoutDiscount driver plan dutyDate registrationDateLocal driverFee.numRides) -- handle UDFs
      (finalOrderAmount, offerId, offerTitle) <-
        if null offers.offerResp
          then pure (feeWithoutDiscount, Nothing, Nothing)
          else do
            let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers.offerResp
            pure (bestOffer.finalOrderAmount, Just bestOffer.offerId, bestOffer.offerDescription.title)
      return (feeWithoutDiscount, finalOrderAmount, offerId, offerTitle)

splitPlatformFee :: HighPrecMoney -> HighPrecMoney -> Plan -> DriverFee -> Maybe HighPrecMoney -> [DriverFee]
splitPlatformFee feeWithoutDiscount_ totalFee plan DriverFee {..} maxMandateAmount = do
  let maxAmount = fromMaybe totalFee maxMandateAmount
  let numEntities = totalFee / maxAmount
      remainingFee = totalFee `mod'` maxAmount
      entityList = replicate (floor numEntities) maxAmount ++ [remainingFee | remainingFee > 0]
   in map
        ( \fee -> do
            let (platformFee_, cgst, sgst) = calculatePlatformFeeAttr fee plan
            DriverFee
              { platformFee = PlatformFee {fee = platformFee_, ..},
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

driverFeeSplitter :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => PaymentMode -> Plan -> HighPrecMoney -> HighPrecMoney -> DriverFee -> Maybe (Id Mandate) -> UTCTime -> m ()
driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFee mandateId now = do
  mandate <- maybe (pure Nothing) QMD.findById mandateId
  let splittedFees = splitPlatformFee feeWithoutDiscount (roundToHalf totalFee) plan driverFee (roundToHalf <$> (mandate <&> (.maxAmount)))
  case splittedFees of
    [] -> throwError (InternalError "No driver fee entity with non zero total fee")
    (firstFee : restFees) -> do
      resetFee firstFee.id firstFee.govtCharges firstFee.platformFee.fee firstFee.platformFee.cgst firstFee.platformFee.sgst now
      mapM_ (processRestFee paymentMode) restFees

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
      updateStatus PAYMENT_OVERDUE driverFee.id now
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

updateSerialOrderForInvoicesInWindow :: (MonadFlow m, CacheFlow m r) => Id DriverFee -> Id Merchant -> UTCTime -> UTCTime -> m ()
updateSerialOrderForInvoicesInWindow driverFeeId merchantId startTime endTime = do
  Hedis.whenWithLockRedis (billNumberGenerationLockKey driverFeeId.getId) 60 $ do
    --- change lock based on mechantId --
    counter <- getDriverFeeBillNumberKey merchantId
    when (isNothing counter) $ do
      count <- listToMaybe <$> QDF.findMaxBillNumberInRange merchantId startTime endTime
      void $ Hedis.incrby (mkDriverFeeBillNumberKey merchantId) (maybe 0 toInteger (count >>= (.billNumber)))
    billNumber' <- Hedis.incr (mkDriverFeeBillNumberKey merchantId)
    QDF.updateBillNumberById (Just (fromInteger billNumber')) driverFeeId

mkInvoiceAgainstDriverFee ::
  ( MonadFlow m
  ) =>
  DriverFee ->
  m INV.Invoice
mkInvoiceAgainstDriverFee driverFee = do
  invoiceId <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  return $
    INV.Invoice
      { id = Id invoiceId,
        invoiceShortId = shortId.getShortId,
        driverFeeId = driverFee.id,
        invoiceStatus = INV.ACTIVE_INVOICE,
        paymentMode = INV.AUTOPAY_INVOICE,
        bankErrorCode = Nothing,
        bankErrorMessage = Nothing,
        bankErrorUpdatedAt = Nothing,
        maxMandateAmount = Nothing,
        driverId = driverFee.driverId,
        lastStatusCheckedAt = Nothing,
        updatedAt = now,
        createdAt = now
      }

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, HasField "schedulerSetName" r Text, HasField "schedulerType" r SchedulerType, HasField "jobInfoMap" r (M.Map Text Bool)) => TransporterConfig -> UTCTime -> UTCTime -> Id Merchant -> Int -> m ()
scheduleJobs transporterConfig startTime endTime merchantId maxShards = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let dfNotificationTime = transporterConfig.driverAutoPayNotificationTime
  let dfCalculationJobTs = diffUTCTime (addUTCTime dfNotificationTime endTime) now
  createJobIn @_ @'SendPDNNotificationToDriver dfCalculationJobTs maxShards $
    SendPDNNotificationToDriverJobData
      { merchantId = merchantId,
        startTime = startTime,
        endTime = endTime
      }
  createJobIn @_ @'SendOverlay dfCalculationJobTs maxShards $
    SendOverlayJobData
      { merchantId = merchantId,
        rescheduleInterval = Nothing,
        overlayKey = manualInvoiceGeneratedNudgeKey,
        udf1 = Just $ show MANUAL,
        condition = InvoiceGenerated MANUAL,
        scheduledTime = TimeOfDay 0 0 0, -- won't be used as rescheduleInterval is Nothing
        freeTrialDays = transporterConfig.freeTrialDays,
        timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
        driverPaymentCycleDuration = transporterConfig.driverPaymentCycleDuration,
        driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime
      }
  createJobIn @_ @'SendOverlay dfCalculationJobTs maxShards $
    SendOverlayJobData
      { merchantId = merchantId,
        rescheduleInterval = Nothing,
        overlayKey = autopayInvoiceGeneratedNudgeKey,
        udf1 = Just $ show AUTOPAY,
        condition = InvoiceGenerated AUTOPAY,
        scheduledTime = TimeOfDay 0 0 0, -- won't be used as rescheduleInterval is Nothing
        freeTrialDays = transporterConfig.freeTrialDays,
        timeDiffFromUtc = transporterConfig.timeDiffFromUtc,
        driverPaymentCycleDuration = transporterConfig.driverPaymentCycleDuration,
        driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime
      }

manualInvoiceGeneratedNudgeKey :: Text
manualInvoiceGeneratedNudgeKey = "INVOICE_GENERATED_MANUAL"

autopayInvoiceGeneratedNudgeKey :: Text
autopayInvoiceGeneratedNudgeKey = "INVOICE_GENERATED_AUTOPAY"
