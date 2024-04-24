{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.DriverFeeUpdates.DriverFee
  ( calculateDriverFeeForDrivers,
    sendManualPaymentLink,
    getsetManualLinkErrorTrackingKey,
  )
where

import qualified Control.Monad.Catch as C
import Control.Monad.Extra (mapMaybeM)
import Data.Fixed (mod')
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Driver as DDriver
import Domain.Action.UI.Ride.EndRide.Internal (getDriverFeeBillNumberKey, getDriverFeeCalcJobCache, getPlan, mkDriverFee, mkDriverFeeBillNumberKey, setDriverFeeBillNumberKey, setDriverFeeCalcJobCache)
import Domain.Types.DriverFee
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantMessage (MessageKey (..))
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.Overlay (OverlayCondition (..))
import Domain.Types.Merchant.TransporterConfig (TransporterConfig)
import Domain.Types.Person
import Domain.Types.Plan (BasedOnEntity (..), PaymentMode (AUTOPAY, MANUAL), Plan (..), PlanBaseAmount (..), ServiceNames (..))
import Domain.Types.SubscriptionConfig
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id (Id (Id), cast, getShortId)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import SharedLogic.DriverFee (calcNumRides, calculatePlatformFeeAttr, roundToHalf, setCoinToCashUsedAmount, setCreateDriverFeeForServiceInSchedulerKey, toCreateDriverFeeForService)
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.Queries.DriverFee as QDF
import Storage.Queries.DriverInformation (updatePendingPayment, updateSubscription)
import Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QMD
import qualified Storage.Queries.Person as QP
import qualified Tools.Payment as TPayment

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
      mbMerchantOpCityId = jobData.merchantOperatingCityId
      startTime = jobData.startTime
      endTime = jobData.endTime
      serviceName = fromMaybe YATRI_SUBSCRIPTION jobData.serviceName
      applyOfferCall = TPayment.offerApply merchantId
  now <- getCurrentTime
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  subscriptionConfigs <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName)
  driverFees <- getOrGenerateDriverFeeDataBasedOnServiceName serviceName startTime endTime merchantId merchantOpCityId transporterConfig
  let threshold = transporterConfig.driverFeeRetryThresholdConfig
  driverFeesToProccess <-
    mapMaybeM
      ( \driverFee -> do
          let count = driverFee.schedulerTryCount
              driverFeeId = driverFee.id
          if count > threshold
            then do
              QDF.updateStatus MANUAL_REVIEW_NEEDED driverFeeId now
              return Nothing
            else do
              QDF.updateRetryCount (count + 1) driverFeeId
              return (Just driverFee)
      )
      driverFees
  flip C.catchAll (\e -> C.mask_ $ logError $ "Driver fee scheduler for merchant id " <> merchantId.getId <> " failed. Error: " <> show e) $ do
    for_ driverFeesToProccess $ \driverFee -> do
      mbDriverPlan <- findByDriverIdWithServiceName (cast driverFee.driverId) serviceName
      mbPlan <- getPlan mbDriverPlan serviceName merchantOpCityId
      mbDriverStat <- QDS.findById (cast driverFee.driverId)
      case mbPlan of
        Nothing -> pure ()
        Just plan -> do
          let (planBaseFrequcency, baseAmount) = getFreqAndBaseAmountcase plan.planBaseAmount
              dutyDate = driverFee.createdAt
              (mandateSetupDate, mandateId) = case mbDriverPlan of
                Nothing -> (now, Nothing)
                Just driverPlan -> (fromMaybe now driverPlan.mandateSetupDate, driverPlan.mandateId)
              coinCashLeft = if plan.eligibleForCoinDiscount then max 0.0 $ maybe 0.0 (.coinCovertedToCashLeft) mbDriverStat else 0.0

          driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
          let numRidesForPlanCharges = calcNumRides driverFee transporterConfig - plan.freeRideCount
          --------- calculations based of frequency happens here ------------
          (feeWithoutDiscount, totalFee, offerId, offerTitle) <- do
            calcFinalOrderAmounts merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges planBaseFrequcency baseAmount driverFee
          ---------------------------------------------------------------------
          ------------- update driver fee with offer and plan details ---------
          let offerAndPlanTitle = Just plan.name <> Just "-*@*-" <> offerTitle ---- this we will send in payment history ----
          updateOfferAndPlanDetails offerId offerAndPlanTitle (Just plan.id) (Just plan.paymentMode) driverFee.id
          let driverFeeUpdateWithPlanAndOffer =
                driverFee
                  { offerId = offerId,
                    planOfferTitle = offerAndPlanTitle,
                    planId = Just plan.id,
                    planMode = Just plan.paymentMode
                  }
          --------------------------------------------------

          fork "Applying offer" $ do
            offerTxnId <- getShortId <$> generateShortId
            let offerApplied = catMaybes [offerId]
                offerApplyRequest' = mkApplyOfferRequest offerTxnId offerApplied feeWithoutDiscount plan driverFee.driverId dutyDate mandateSetupDate numRidesForPlanCharges
            maybe (pure ()) (\offerRequest -> do void $ try @_ @SomeException $ withShortRetry (applyOfferCall merchantOpCityId subscriptionConfigs.paymentServiceName offerRequest)) (Just offerApplyRequest')

          let paymentMode = maybe MANUAL (.planType) mbDriverPlan
          ------------- process driver fee based on payment mode ----------------
          unless (totalFee == 0) $ do
            -- driverFeeUpdateWithPlanAndOffer <- QDF.findById driverFee.id >>= fromMaybeM (InternalError $ "driverFee not found with driverFee id : " <> driverFee.id.getId)
            if coinCashLeft >= totalFee
              then do
                void $ QDS.updateCoinToCashByDriverId (cast driverFeeUpdateWithPlanAndOffer.driverId) (-1.0 * totalFee)
                setCoinToCashUsedAmount driverFeeUpdateWithPlanAndOffer totalFee
                QDF.updateStatusByIds CLEARED_BY_YATRI_COINS [driverFeeUpdateWithPlanAndOffer.id] now
                driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFeeUpdateWithPlanAndOffer mandateId Nothing subscriptionConfigs now
                invoice <- mkInvoiceAgainstDriverFee driverFeeUpdateWithPlanAndOffer (True, paymentMode == AUTOPAY)
                updateAmountPaidByCoins (Just totalFee) driverFeeUpdateWithPlanAndOffer.id
                QINV.create invoice
              else do
                when (coinCashLeft > 0) $ do
                  QDS.updateCoinToCashByDriverId (cast driverFeeUpdateWithPlanAndOffer.driverId) (-1.0 * coinCashLeft)
                  setCoinToCashUsedAmount driverFeeUpdateWithPlanAndOffer coinCashLeft
                driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFeeUpdateWithPlanAndOffer mandateId (Just coinCashLeft) subscriptionConfigs now
                updatePendingPayment True (cast driverFeeUpdateWithPlanAndOffer.driverId)
          -------------------------------------------------------------------------------
          -- blocking
          dueDriverFees <- QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName (cast driverFee.driverId) serviceName -- Problem with lazy evaluation?
          let driverFeeIds = map (.id) dueDriverFees
              due = sum $ map (\fee -> if (fee.startTime /= startTime && fee.endTime /= endTime) then roundToHalf $ fromIntegral fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst else 0) dueDriverFees
          if roundToHalf (due + totalFee - min coinCashLeft totalFee) >= plan.maxCreditLimit
            then do
              mapM_ updateDriverFeeToManual driverFeeIds
              updateDriverFeeToManual driverFee.id
              when plan.subscribedFlagToggleAllowed $ updateSubscription False (cast driverFee.driverId)
            else do
              unless (totalFee == 0 || coinCashLeft >= totalFee) $ processDriverFee paymentMode driverFee subscriptionConfigs
          updateSerialOrderForInvoicesInWindow driverFee.id merchantOpCityId startTime endTime serviceName

  case listToMaybe driverFees of
    Nothing -> do
      Hedis.del (mkDriverFeeBillNumberKey merchantOpCityId serviceName)
      maxShards <- asks (.maxShards)
      scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId maxShards serviceName subscriptionConfigs jobData
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

processDriverFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  PaymentMode ->
  DriverFee ->
  SubscriptionConfig ->
  m ()
processDriverFee paymentMode driverFee subscriptionConfig = do
  now <- getCurrentTime
  case paymentMode of
    MANUAL -> do
      ( if subscriptionConfig.allowManualPaymentLinks
          then
            ( do
                updateStatus PAYMENT_PENDING driverFee.id now
                updateFeeType RECURRING_INVOICE driverFee.id
            )
          else
            ( do
                updateDriverFeeToManual driverFee.id
            )
        )
    AUTOPAY -> do
      updateStatus PAYMENT_PENDING driverFee.id now
      updateFeeType RECURRING_EXECUTION_INVOICE driverFee.id
      invoice <- mkInvoiceAgainstDriverFee driverFee (False, True)
      QINV.create invoice
      QDF.updateAutopayPaymentStageById (Just NOTIFICATION_SCHEDULED) (Just now) driverFee.id

processRestFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  PaymentMode ->
  DriverFee ->
  SubscriptionConfig ->
  m ()
processRestFee paymentMode DriverFee {..} subscriptionConfig = do
  id_ <- generateGUID
  let driverFee =
        DriverFee
          { id = id_,
            status = if paymentMode == MANUAL && not (subscriptionConfig.allowManualPaymentLinks) then PAYMENT_OVERDUE else PAYMENT_PENDING,
            feeType = if paymentMode == MANUAL then RECURRING_INVOICE else RECURRING_EXECUTION_INVOICE,
            ..
          }
  QDF.create driverFee
  processDriverFee paymentMode driverFee subscriptionConfig
  updateSerialOrderForInvoicesInWindow driverFee.id merchantOperatingCityId startTime endTime driverFee.serviceName

makeOfferReq :: HighPrecMoney -> Person -> Plan -> UTCTime -> UTCTime -> Int -> TransporterConfig -> Payment.OfferListReq
makeOfferReq totalFee driver plan dutyDate registrationDate numOfRides transporterConfig = do
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = totalFee, currency = Payment.INR} -- add UDFs
      customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
  Payment.OfferListReq
    { order = offerOrder,
      customer = Just customerReq,
      planId = plan.id.getId,
      registrationDate,
      paymentMode = show plan.paymentMode,
      dutyDate,
      numOfRides,
      offerListingMetric = if transporterConfig.enableUdfForOffers then Just Payment.IS_APPLICABLE else Nothing
    }

getFinalOrderAmount ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  HighPrecMoney ->
  Id Merchant ->
  TransporterConfig ->
  Person ->
  Plan ->
  UTCTime ->
  Int ->
  DriverFee ->
  m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan registrationDate numOfRidesConsideredForCharges driverFee = do
  now <- getCurrentTime
  let dutyDate = driverFee.createdAt
      registrationDateLocal = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) registrationDate
      feeWithOutDiscountPlusSpecialZone = feeWithoutDiscount + driverFee.specialZoneAmount
  if feeWithOutDiscountPlusSpecialZone == 0
    then do
      updateCollectedPaymentStatus CLEARED Nothing now driverFee.id
      return (0, 0, Nothing, Nothing)
    else do
      offers <- SPayment.offerListCache merchantId driver.merchantOperatingCityId plan.serviceName (makeOfferReq feeWithoutDiscount driver plan dutyDate registrationDateLocal numOfRidesConsideredForCharges transporterConfig) -- handle UDFs
      (finalOrderAmount, offerId, offerTitle) <-
        if null offers.offerResp
          then pure (feeWithoutDiscount, Nothing, Nothing)
          else do
            let bestOffer = minimumBy (comparing (.finalOrderAmount)) offers.offerResp
            pure (bestOffer.finalOrderAmount, Just bestOffer.offerId, bestOffer.offerDescription.title)
      if finalOrderAmount + driverFee.specialZoneAmount == 0
        then do
          updateCollectedPaymentStatus CLEARED offerId now driverFee.id
          updateFeeWithoutDiscount (Just feeWithOutDiscountPlusSpecialZone) driverFee.id
          return (0, 0, offerId, offerTitle)
        else return (feeWithOutDiscountPlusSpecialZone, finalOrderAmount + driverFee.specialZoneAmount, offerId, offerTitle)

splitPlatformFee :: HighPrecMoney -> HighPrecMoney -> Plan -> DriverFee -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> [DriverFee]
splitPlatformFee feeWithoutDiscount_ totalFee plan DriverFee {..} maxAmountPerDriverfeeThreshold coinClearedAmount = do
  let maxAmount = fromMaybe totalFee maxAmountPerDriverfeeThreshold
  let numEntities = totalFee / maxAmount
      remainingFee = totalFee `mod'` maxAmount
      coinDiscount = if remainingFee <= 0 then coinClearedAmount else Nothing
      entityList = replicate (floor numEntities) (maxAmount, coinDiscount) ++ [(remainingFee, coinClearedAmount) | remainingFee > 0]
   in map
        ( \(fee, coinPaidAmount) -> do
            let (platformFee_, cgst, sgst) = calculatePlatformFeeAttr fee plan
            DriverFee
              { platformFee = PlatformFee {fee = platformFee_, ..},
                feeType = feeType,
                feeWithoutDiscount = Just feeWithoutDiscount_, -- same for all splitted ones, not remaining fee
                amountPaidByCoin = coinPaidAmount,
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

driverFeeSplitter ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  PaymentMode ->
  Plan ->
  HighPrecMoney ->
  HighPrecMoney ->
  DriverFee ->
  Maybe (Id Mandate) ->
  Maybe HighPrecMoney ->
  SubscriptionConfig ->
  UTCTime ->
  m ()
driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFee mandateId mbCoinAmountUsed subscriptionConfigs now = do
  mandate <- maybe (pure Nothing) QMD.findById mandateId
  let amountForSpiltting = if isNothing mbCoinAmountUsed then Just $ roundToHalf totalFee else roundToHalf <$> (mandate <&> (.maxAmount))
      coinAmountUsed = fromMaybe 0 mbCoinAmountUsed
      totalFeeWithCoinDeduction = roundToHalf $ totalFee - coinAmountUsed
      splittedFees = splitPlatformFee feeWithoutDiscount totalFeeWithCoinDeduction plan driverFee amountForSpiltting mbCoinAmountUsed
  case splittedFees of
    [] -> throwError (InternalError "No driver fee entity with non zero total fee")
    (firstFee : restFees) -> do
      resetFee firstFee.id firstFee.govtCharges firstFee.platformFee (Just feeWithoutDiscount) firstFee.amountPaidByCoin now
      mapM_ (\dfee -> processRestFee paymentMode dfee subscriptionConfigs) restFees

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

updateSerialOrderForInvoicesInWindow ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DriverFee ->
  Id MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  ServiceNames ->
  m ()
updateSerialOrderForInvoicesInWindow driverFeeId merchantOpCityId startTime endTime serviceName = do
  Hedis.whenWithLockRedis (billNumberGenerationLockKey driverFeeId.getId) 60 $ do
    --- change lock based on mechantId --
    counter <- getDriverFeeBillNumberKey merchantOpCityId serviceName
    when (isNothing counter) $ do
      count <- listToMaybe <$> QDF.findMaxBillNumberInRangeForServiceName merchantOpCityId startTime endTime serviceName
      void $ Hedis.incrby (mkDriverFeeBillNumberKey merchantOpCityId serviceName) (maybe 0 toInteger (count >>= (.billNumber)))
    billNumber' <- Hedis.incr (mkDriverFeeBillNumberKey merchantOpCityId serviceName)
    QDF.updateBillNumberById (Just (fromInteger billNumber')) driverFeeId

getOrGenerateDriverFeeDataBasedOnServiceName ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  UTCTime ->
  UTCTime ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  TransporterConfig ->
  m [DriverFee]
getOrGenerateDriverFeeDataBasedOnServiceName serviceName startTime endTime merchantId merchantOperatingCityId transporterConfig = do
  now <- getCurrentTime
  case serviceName of
    YATRI_SUBSCRIPTION -> QDF.findAllFeesInRangeWithStatusAndServiceName (Just merchantId) merchantOperatingCityId startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize serviceName
    YATRI_RENTAL -> do
      when (startTime >= endTime) $ throwError (InternalError "Invalid time range for driver fee calculation")
      let mbStartTime = Just startTime
          mbEndTime = Just endTime
      mbtoCreateDriverFeeForService <- toCreateDriverFeeForService serviceName merchantOperatingCityId
      if fromMaybe True mbtoCreateDriverFeeForService
        then do
          driverEligibleForRentals <- findAllDriversEligibleForService serviceName merchantId merchantOperatingCityId
          driverFees <-
            mapMaybeM
              ( \dPlan -> do
                  mbExistingDFee <- QDF.findFeeByDriverIdAndServiceNameInRange (cast dPlan.driverId) serviceName startTime endTime
                  if isNothing mbExistingDFee
                    then do
                      currency <- SMerchant.getCurrencyByMerchantOpCity merchantOperatingCityId
                      driverFee <- mkDriverFee serviceName now mbStartTime mbEndTime merchantId dPlan.driverId Nothing 0 0.0 0.0 0.0 currency transporterConfig Nothing
                      QDF.create driverFee
                      return $ Just driverFee
                    else return Nothing
              )
              driverEligibleForRentals
          setCreateDriverFeeForServiceInSchedulerKey serviceName merchantOperatingCityId False
          return $ maybe driverFees (\limit -> take limit driverFees) transporterConfig.driverFeeCalculatorBatchSize
        else QDF.findAllFeesInRangeWithStatusAndServiceName (Just merchantId) merchantOperatingCityId startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize serviceName

mkInvoiceAgainstDriverFee ::
  ( MonadFlow m
  ) =>
  DriverFee ->
  (Bool, Bool) ->
  m INV.Invoice
mkInvoiceAgainstDriverFee driverFee (isCoinCleared, isAutoPay) = do
  invoiceId <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  return $
    INV.Invoice
      { id = Id invoiceId,
        invoiceShortId = shortId.getShortId,
        driverFeeId = driverFee.id,
        invoiceStatus = if isCoinCleared then INV.CLEARED_BY_YATRI_COINS else INV.ACTIVE_INVOICE,
        paymentMode = if isCoinCleared && not isAutoPay then INV.MANUAL_INVOICE else INV.AUTOPAY_INVOICE,
        bankErrorCode = Nothing,
        bankErrorMessage = Nothing,
        bankErrorUpdatedAt = Nothing,
        maxMandateAmount = Nothing,
        driverId = driverFee.driverId,
        lastStatusCheckedAt = Nothing,
        serviceName = driverFee.serviceName,
        merchantOperatingCityId = driverFee.merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }

scheduleJobs ::
  (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) =>
  TransporterConfig ->
  UTCTime ->
  UTCTime ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  Int ->
  ServiceNames ->
  SubscriptionConfig ->
  CalculateDriverFeesJobData ->
  m ()
scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId maxShards serviceName subscriptionConfigs jobData = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let dfNotificationTime = transporterConfig.driverAutoPayNotificationTime
  let timeDiffNormalFlow = addUTCTime dfNotificationTime endTime
  let timeDiffNormalFlowLinkSendJob = addUTCTime subscriptionConfigs.paymentLinkJobTime endTime
  let dfCalculationJobTs = if timeDiffNormalFlow > now then diffUTCTime timeDiffNormalFlow now else 5 * 60 --- 5 min
  let paymentLinkSendJobTs = if timeDiffNormalFlowLinkSendJob > now then diffUTCTime timeDiffNormalFlowLinkSendJob now else 5 * 60 --- 5 min
      scheduleChildJobs = fromMaybe True jobData.createChildJobs
      scheduleNotification = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleNotification
      scheduleOverlay = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleOverlay
      scheduleManualPaymentLink = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleManualPaymentLink
      scheduleDriverFeeCalc = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleDriverFeeCalc
  when scheduleNotification $ do
    createJobIn @_ @'SendPDNNotificationToDriver dfCalculationJobTs maxShards $
      SendPDNNotificationToDriverJobData
        { merchantId = merchantId,
          merchantOperatingCityId = Just merchantOpCityId,
          startTime = startTime,
          endTime = endTime,
          retryCount = Just 0,
          serviceName = Just serviceName
        }
  when (subscriptionConfigs.useOverlayService && scheduleOverlay) $ do
    createJobIn @_ @'SendOverlay (dfCalculationJobTs + 5400) maxShards $
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
          driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime,
          driverFeeOverlaySendingTimeLimitInDays = transporterConfig.driverFeeOverlaySendingTimeLimitInDays,
          merchantOperatingCityId = Just merchantOpCityId,
          overlayBatchSize = transporterConfig.overlayBatchSize,
          serviceName = Just serviceName
        }
    createJobIn @_ @'SendOverlay (dfCalculationJobTs + 5400) maxShards $
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
          driverPaymentCycleStartTime = transporterConfig.driverPaymentCycleStartTime,
          merchantOperatingCityId = Just merchantOpCityId,
          driverFeeOverlaySendingTimeLimitInDays = transporterConfig.driverFeeOverlaySendingTimeLimitInDays,
          overlayBatchSize = transporterConfig.overlayBatchSize,
          serviceName = Just serviceName
        }
  when (subscriptionConfigs.allowManualPaymentLinks && scheduleManualPaymentLink) $ do
    createJobIn @_ @'SendManualPaymentLink paymentLinkSendJobTs maxShards $
      SendManualPaymentLinkJobData
        { merchantId = merchantId,
          merchantOperatingCityId = merchantOpCityId,
          serviceName = serviceName,
          startTime = startTime,
          endTime = endTime,
          channel = subscriptionConfigs.paymentLinkChannel
        }
  when (subscriptionConfigs.allowDriverFeeCalcSchedule && scheduleDriverFeeCalc) $ do
    let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay endTime) (secondsToDiffTime 0))
        startTime' = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
        endTime' = addUTCTime transporterConfig.driverPaymentCycleDuration startTime'
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> pure ()
      Just dfCalcTime -> do
        isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache startTime' endTime' merchantOpCityId serviceName
        let dfCalculationJobTs' = diffUTCTime (addUTCTime dfCalcTime endTime') now
        case isDfCaclculationJobScheduled of
          ----- marker ---
          Nothing -> do
            createJobIn @_ @'CalculateDriverFees dfCalculationJobTs' maxShards $
              CalculateDriverFeesJobData
                { merchantId = merchantId,
                  merchantOperatingCityId = Just merchantOpCityId,
                  startTime = startTime',
                  serviceName = Just serviceName,
                  endTime = endTime',
                  scheduleNotification = Just True,
                  scheduleOverlay = Just True,
                  scheduleManualPaymentLink = Just True,
                  scheduleDriverFeeCalc = Just True,
                  createChildJobs = Just True
                }
            setDriverFeeCalcJobCache startTime endTime' merchantOpCityId serviceName dfCalculationJobTs
            setCreateDriverFeeForServiceInSchedulerKey serviceName merchantOpCityId True
            setDriverFeeBillNumberKey merchantOpCityId 1 36000 serviceName
          _ -> pure ()

calcFinalOrderAmounts ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Id Merchant ->
  TransporterConfig ->
  Person ->
  Plan ->
  UTCTime ->
  Int ->
  Text ->
  HighPrecMoney ->
  DriverFee ->
  m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
calcFinalOrderAmounts merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges planBaseFrequcency baseAmount driverFee =
  case (planBaseFrequcency, plan.basedOnEntity) of
    ("PER_RIDE", RIDE) -> do
      let feeWithoutDiscount = max 0 (min plan.maxAmount (baseAmount * HighPrecMoney (toRational numRidesForPlanCharges)))
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee
    ("DAILY", RIDE) -> do
      let feeWithoutDiscount = if numRidesForPlanCharges > 0 then baseAmount else 0
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee
    ("DAILY", NONE) -> do
      let feeWithoutDiscount = baseAmount
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee
    _ -> return (0.0, 0.0, Nothing, Nothing) -- TODO: handle WEEKLY and MONTHLY later

manualInvoiceGeneratedNudgeKey :: Text
manualInvoiceGeneratedNudgeKey = "INVOICE_GENERATED_MANUAL"

autopayInvoiceGeneratedNudgeKey :: Text
autopayInvoiceGeneratedNudgeKey = "INVOICE_GENERATED_AUTOPAY"

sendManualPaymentLink ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "smsCfg" r SmsConfig,
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  Job 'SendManualPaymentLink ->
  m ExecutionResult
sendManualPaymentLink Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
      endTime = jobData.endTime
      serviceName = jobData.serviceName
  now <- getCurrentTime
  subscriptionConfigs <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName)
  let deepLinkExpiry = subscriptionConfigs.deepLinkExpiryTimeInMinutes
  driverPlansToProccess <- findAllDriversToSendManualPaymentLinkWithLimit serviceName merchantId opCityId endTime subscriptionConfigs.genericBatchSizeForJobs
  if null driverPlansToProccess
    then return Complete
    else do
      processAndSendManualPaymentLink driverPlansToProccess subscriptionConfigs merchantId opCityId serviceName deepLinkExpiry endTime now
      ReSchedule <$> getRescheduledTime subscriptionConfigs.genericJobRescheduleTime

processAndSendManualPaymentLink ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig) =>
  [DPlan.DriverPlan] ->
  SubscriptionConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  ServiceNames ->
  Maybe Int ->
  UTCTime ->
  UTCTime ->
  m ()
processAndSendManualPaymentLink driverPlansToProccess subscriptionConfigs merchantId opCityId serviceName mbDeepLinkExpiry endTime now = do
  forM_ driverPlansToProccess $ \driverPlanForManualCharge -> do
    let driverId = driverPlanForManualCharge.driverId
    getPendingAndOverDueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [PAYMENT_OVERDUE, PAYMENT_PENDING] serviceName
    updateStatusByIds PAYMENT_OVERDUE (map (.id) $ filter ((== PAYMENT_PENDING) . (.status)) getPendingAndOverDueDriverFees) now
    let allowDeepLink = subscriptionConfigs.sendDeepLink
    let mbDeepLinkData = if allowDeepLink then Just $ SPayment.DeepLinkData {sendDeepLink = Just True, expiryTimeInMinutes = mbDeepLinkExpiry} else Nothing
    if not $ null getPendingAndOverDueDriverFees
      then do
        resp' <- try @_ @SomeException $ DDriver.clearDriverDues (driverId, merchantId, opCityId) serviceName mbDeepLinkData
        errorCatchAndHandle
          driverId
          resp'
          subscriptionConfigs
          endTime
          now
          ( \resp -> do
              let mbPaymentLink = resp.orderResp.payment_links
                  payload = resp.orderResp.sdk_payload.payload
                  mbAmount = readMaybe (T.unpack payload.amount) :: Maybe HighPrecMoney
              whatsAppResp <- try @_ @SomeException $ SPayment.sendLinkTroughChannelProvided mbPaymentLink driverId mbAmount (Just subscriptionConfigs.paymentLinkChannel) allowDeepLink WHATSAPP_SEND_MANUAL_PAYMENT_LINK
              errorCatchAndHandle driverId whatsAppResp subscriptionConfigs endTime now (\_ -> QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName driverId serviceName endTime)
          )
      else do
        QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName driverId serviceName endTime

errorCatchAndHandle ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig) =>
  Id Driver ->
  Either SomeException a ->
  SubscriptionConfig ->
  UTCTime ->
  UTCTime ->
  (a -> m ()) ->
  m ()
errorCatchAndHandle driverId resp' subscriptionConfig endTime _ function = do
  case resp' of
    Left _ -> do
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkManualLinkErrorTrackingByDriverIdKey driverId) subscriptionConfig.maxRetryCount
      if eligibleForRetryInNextBatch
        then return ()
        else QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName driverId subscriptionConfig.serviceName endTime
    Right resp -> function resp

isEligibleForRetryInNextBatch :: (MonadFlow m, CacheFlow m r) => Text -> Int -> m Bool
isEligibleForRetryInNextBatch key maxCount = do
  count <-
    Hedis.get key >>= \case
      Just count -> return count
      Nothing -> return 0
  if count < maxCount
    then do
      void $ Hedis.incr key
      Hedis.expire key (60 * 60 * 6) ---- 6 hours expiry
      return True
    else return False

mkManualLinkErrorTrackingByDriverIdKey :: Id Driver -> Text
mkManualLinkErrorTrackingByDriverIdKey driverId = "ErrorRetryCountFor:DriverId:" <> driverId.getId

getsetManualLinkErrorTrackingKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Maybe Int)
getsetManualLinkErrorTrackingKey driverId = Hedis.get (mkManualLinkErrorTrackingByDriverIdKey driverId)
