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
    getPlan,
  )
where

import qualified Control.Monad.Catch as C
import Data.Fixed (mod')
import Data.Ord
import Domain.Action.UI.Ride.EndRide.Internal (getDriverFeeCalcJobFlagKey, mkDriverFeeCalcJobFlagKey)
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import Domain.Types.DriverFee
import Domain.Types.DriverPlan (DriverPlan)
import Domain.Types.Merchant
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import Domain.Types.Plan (PaymentMode (AUTOPAY, MANUAL), Plan (..), PlanBaseAmount (..), PlanType (DEFAULT))
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id (Id, cast)
import Kernel.Utils.Common (CacheFlow, EncFlow, EsqDBFlow, GuidLike (generateGUID), HasShortDurationRetryCfg, HighPrecMoney (..), Log (withLogTag), MonadFlow, MonadGuid, MonadTime (getCurrentTime), addUTCTime, fromMaybeM, generateShortId, getLocalCurrentTime, logError, logInfo, secondsToNominalDiffTime, throwError, withShortRetry)
import Lib.Scheduler
import SharedLogic.Allocator
import SharedLogic.DriverFee hiding (PlatformFee)
import qualified Storage.CachedQueries.Merchant.TransporterConfig as SCT
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import Storage.Queries.DriverFee as QDF
import Storage.Queries.DriverInformation (updatePendingPayment, updateSubscription)
import Storage.Queries.DriverPlan
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import Tools.Error
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
        overdueFee <- B.runInReplica $ findOldestFeeByStatus (cast person.id) PAYMENT_OVERDUE
        case overdueFee of
          Nothing -> do
            -- Esq.runTransaction $ updateStatus PAYMENT_PENDING driverFee.id now
            _ <- updateStatus PAYMENT_PENDING now driverFee.id
            updatePendingPayment True (cast person.id)
          Just oDFee -> do
            mergeDriverFee oDFee driverFee now
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
    HasShortDurationRetryCfg r c
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
    driverFees <- findFeesInRangeWithStatus merchantId startTime endTime ONGOING Nothing
    for_ driverFees $ \driverFee -> do
      mbDriverPlan <- findByDriverId (cast driverFee.driverId)
      plan <- getPlan mbDriverPlan merchantId
      cacheDriverPlan driverFee.driverId endTime plan
      setDriverFeeBillNumberKey merchantId 1 36000 -- check here --
      Hedis.del (mkDriverFeeCalcJobFlagKey startTime endTime merchantId)
  -- Schedule notif job

  now <- getCurrentTime
  transporterConfig <- SCT.findByMerchantId merchantId >>= fromMaybeM (TransporterConfigNotFound merchantId.getId)
  driverFees <- findFeesInRangeWithStatus merchantId startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize

  for_ driverFees $ \driverFee -> do
    plan <- getDriverPlanCache endTime driverFee.driverId >>= fromMaybeM (InternalError ("No plan found for driver" <> driverFee.driverId.getId))
    driverPlan <- findByDriverId (cast driverFee.driverId) >>= fromMaybeM (InternalError ("No driver plan found for driver" <> driverFee.driverId.getId))
    let (planBaseFrequcency, baseAmount) = getFreqAndBaseAmountcase plan.planBaseAmount
    let due = fromIntegral driverFee.govtCharges + fromIntegral driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst
    driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)

    (totalFee, offerId, offerTitle) <- case planBaseFrequcency of
      "PER_RIDE" -> do
        let numRides = driverFee.numRides - plan.freeRideCount
            feeWithoutDiscount = min plan.maxAmount (baseAmount * HighPrecMoney (toRational numRides))
        getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount driverFee.id
      "DAILY" -> do
        let numRides = driverFee.numRides - plan.freeRideCount
            feeWithoutDiscount = if numRides > 0 then baseAmount else 0
        getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount driverFee.id
      _ -> return (0, Nothing, Nothing) -- TODO: handle WEEKLY and MONTHLY later
    let offerAndPlanTitle = Just plan.id.getId <> Just "-*$*-" <> offerTitle ---- this we will send in payment history ----
    updateOfferAndPlanDetails offerId offerAndPlanTitle driverFee.id now
    offerTxnId <- generateShortId
    let offerApplyRequest' = mkApplyOfferRequest offerTxnId.getShortId (catMaybes [offerId]) due driverPlan <$> driverPlan.mandateSetupDate
    maybe (pure ()) (\offerRequest -> do void $ try @_ @SomeException $ withShortRetry (applyOfferCall offerRequest)) offerApplyRequest'
    ---- here we need the status to notification scheduled ----
    unless (totalFee == 0) $ do
      driverFeeSplitter plan totalFee transporterConfig driverFee now
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
      return Complete
    _ -> case transporterConfig.driverFeeCalculatorBatchGap of
      Nothing -> throwError $ InternalError "No batch gap defined for driver fee calculator job"
      Just gap -> ReSchedule <$> getRescheduledTime gap
  where
    mkApplyOfferRequest offerTxnUUID appliedOfferIds due driverPlan' registrationDate =
      PaymentInterface.OfferApplyReq
        { txnId = offerTxnUUID,
          offers = appliedOfferIds,
          customerId = driverPlan'.driverId.getId,
          amount = due,
          currency = PaymentInterface.INR,
          planId = driverPlan'.planId.getId,
          registrationDate,
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

calculatePlatformFeeAttr :: HighPrecMoney -> TransporterConfig -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
calculatePlatformFeeAttr totalFee transporterConfig = do
  let platformFee = totalFee / HighPrecMoney (toRational $ 1 + transporterConfig.cgstPercentage + transporterConfig.sgstPercentage) -- this should be changed to HighPrecMoney
      cgst = HighPrecMoney (toRational transporterConfig.cgstPercentage) * platformFee
      sgst = HighPrecMoney (toRational transporterConfig.sgstPercentage) * platformFee
  (platformFee, cgst, sgst)

makeOfferReq :: UTCTime -> HighPrecMoney -> Person -> Plan -> Payment.OfferListReq
makeOfferReq now totalFee driver plan = do
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = totalFee, currency = Payment.INR} -- add UDFs
      customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
  Payment.OfferListReq
    { order = offerOrder,
      customer = Just customerReq,
      planId = plan.id.getId,
      registrationDate = now,
      paymentMode = show plan.paymentMode
    }

getFinalOrderAmount :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => HighPrecMoney -> Id Merchant -> TransporterConfig -> Person -> Plan -> HighPrecMoney -> Id DriverFee -> m (HighPrecMoney, Maybe Text, Maybe Text)
getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan baseAmount driverFeeId = do
  now <- getCurrentTime
  let nowLocal = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now
  if feeWithoutDiscount == 0
    then do
      updateStatus CLEARED now driverFeeId
      return (0, Nothing, Nothing)
    else do
      offers <- Payment.offerList merchantId (makeOfferReq nowLocal feeWithoutDiscount driver plan) -- handle UDFs
      (finalOrderAmount, offerId, offerTitle) <-
        if null offers.offerResp
          then pure (baseAmount, Nothing, Nothing)
          else do
            let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers.offerResp
            pure (bestOffer.finalOrderAmount, Just bestOffer.offerId, bestOffer.offerDescription.title)
      let (platformFee, cgst, sgst) = calculatePlatformFeeAttr finalOrderAmount transporterConfig -- this should be HighPrecMoney
      updateFee driverFeeId Nothing 0 (round platformFee) cgst sgst now False -- add split logic before update
      return (finalOrderAmount, offerId, offerTitle)

splitPlatformFee :: HighPrecMoney -> HighPrecMoney -> TransporterConfig -> DriverFee -> [DriverFee]
splitPlatformFee totalFee maxFeePerEntity transporterConfig DriverFee {..} =
  let numEntities = totalFee / maxFeePerEntity
      remainingFee = totalFee `mod'` maxFeePerEntity
      entityList = replicate (floor numEntities) maxFeePerEntity ++ [remainingFee | remainingFee > 0]
   in map
        ( \fee -> do
            let (platformFee_, cgst, sgst) = calculatePlatformFeeAttr fee transporterConfig
            DriverFee
              { platformFee = PlatformFee {fee = round platformFee_, ..},
                feeType = feeType,
                ..
              }
        )
        -- govt_charges, num_rides, total_earnings are same for all these
        entityList

getPlan :: (MonadFlow m, CacheFlow m r) => Maybe DriverPlan -> Id Merchant -> m Plan
getPlan mbDriverPlan merchantId = do
  case mbDriverPlan of
    Just dp -> CQP.findByIdAndPaymentMode dp.planId dp.planType >>= fromMaybeM (PlanNotFound dp.planId.getId)
    Nothing -> do
      plans <- CQP.findByMerchantIdAndType merchantId DEFAULT
      case plans of
        [] -> throwError $ InternalError "No default plan found"
        [pl] -> pure pl
        _ -> throwError $ InternalError "Multiple default plans found"

getFreqAndBaseAmountcase :: PlanBaseAmount -> (Text, HighPrecMoney)
getFreqAndBaseAmountcase planBaseAmount = case planBaseAmount of
  PERRIDE_BASE amount -> ("PER_RIDE" :: Text, amount)
  DAILY_BASE amount -> ("DAILY" :: Text, amount)
  WEEKLY_BASE amount -> ("WEEKLY" :: Text, amount)
  MONTHLY_BASE amount -> ("MONTHLY" :: Text, amount)

driverFeeSplitter :: (MonadFlow m) => Plan -> HighPrecMoney -> TransporterConfig -> DriverFee -> UTCTime -> m ()
driverFeeSplitter plan totalFee transporterConfig driverFee now = do
  case plan.paymentMode of
    MANUAL -> do
      let splittedFees = splitPlatformFee totalFee plan.maxAmount transporterConfig driverFee
      case splittedFees of
        [] -> throwError (InternalError "No driver fee entity with non zero total fee")
        (firstFee : restFees) -> do
          updateStatus PAYMENT_OVERDUE now firstFee.id
          updRestFees <- mapM (buildRestFees PAYMENT_OVERDUE RECURRING_INVOICE) restFees
          createMany updRestFees
    AUTOPAY -> do
      let splittedFees = splitPlatformFee totalFee plan.maxAmount transporterConfig driverFee
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

calcDriverFeeAttr :: (MonadFlow m) => Id Merchant -> DriverFeeStatus -> UTCTime -> UTCTime -> m [(DriverFee, Maybe Person)]
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
    let setCounter = setDriverFeeBillNumberKey merchantId
    case counter of
      Just billNumber -> do
        QDF.updateBillNumberById counter driverFeeId
        Hedis.del (mkDriverFeeBillNumberKey merchantId)
        setCounter (billNumber + 1) 5000
      Nothing -> do
        count <- listToMaybe <$> QDF.findMaxBillNumberInRange merchantId startTime endTime
        Hedis.del (mkDriverFeeBillNumberKey merchantId)
        maybe (pure ()) (\billNumber' -> setCounter (billNumber' + 1) 5000) (count >>= (.billNumber))
