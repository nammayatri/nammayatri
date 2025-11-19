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
    updateCancellationPenaltyAccumulationFees,
  )
where

import qualified Control.Monad.Catch as C
import Control.Monad.Extra (mapMaybeM)
import Data.Fixed (mod')
import Data.List (nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Ord
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Domain.Action.UI.Driver as DDriver
import Domain.Action.UI.Ride.EndRide.Internal (getDriverFeeBillNumberKey, getDriverFeeCalcJobCache, getPlan, mkDriverFee, mkDriverFeeBillNumberKey, setDriverFeeBillNumberKey, setDriverFeeCalcJobCache)
import Domain.Types.DriverFee
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (Mandate)
import Domain.Types.Merchant
import Domain.Types.MerchantMessage (MessageKey (..))
import Domain.Types.MerchantOperatingCity
import Domain.Types.Overlay (OverlayCondition (..))
import Domain.Types.Person
import Domain.Types.Plan (BasedOnEntity (..), PaymentMode (AUTOPAY, MANUAL), Plan (..), PlanBaseAmount (..), ServiceNames (..))
import Domain.Types.SubscriptionConfig
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VendorFee as DVF
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as PaymentInterface
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id (Id (Id), cast)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.Allocator.Jobs.Overlay.SendOverlay as SLOSO
import SharedLogic.DriverFee (calcNumRides, calculatePlatformFeeAttr, getPaymentModeAndVehicleCategoryKey, jobDuplicationPreventionKey, roundToHalf, setCoinToCashUsedAmount, setCreateDriverFeeForServiceInSchedulerKey, toCreateDriverFeeForService)
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Plan as CQP
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.Queries.DriverFee as QDF
import Storage.Queries.DriverInformation (updatePendingPayment, updateSubscription)
import Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QMD
import qualified Storage.Queries.Person as QP
import Storage.Queries.VendorFee as QVF

calculateDriverFeeForDrivers ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasKafkaProducer r
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
      recalculateManualReview = fromMaybe False jobData.recalculateManualReview
  let jobDataT :: Text = show jobData
  hashedJobData <- getHash jobDataT
  let lockKey = "CalculateDriverFeesScheduler:Lock:" <> hashedJobData
  lockResult <- Hedis.whenWithLockRedisAndReturnValue lockKey 1800 $ do
    now <- getCurrentTime
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId mbMerchantOpCityId merchant Nothing
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    subscriptionConfigs <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId Nothing serviceName >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName)
    driverFees <- getOrGenerateDriverFeeDataBasedOnServiceName serviceName startTime endTime merchantId merchantOpCityId transporterConfig recalculateManualReview subscriptionConfigs
    updateCancellationPenaltyAccumulationFees serviceName transporterConfig merchant.id merchantOpCityId
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
        mbPlanFromDPlan <- getPlan mbDriverPlan serviceName merchantOpCityId (Just recalculateManualReview) (mbDriverPlan >>= (.vehicleCategory))
        let useDriverPlan = ((mbPlanFromDPlan <&> (.merchantOpCityId)) == Just driverFee.merchantOperatingCityId) && ((mbPlanFromDPlan <&> (.vehicleCategory)) == Just driverFee.vehicleCategory)
        mbPlan <- if useDriverPlan then pure mbPlanFromDPlan else maybe (pure Nothing) (\planId' -> CQP.findByIdAndPaymentModeWithServiceName planId' (fromMaybe MANUAL $ mbDriverPlan <&> (.planType)) serviceName) driverFee.planId
        let maxCreditLimitLinkedToDPlan = mbPlan <&> (.maxCreditLimit)
            isPlanToggleAllowedAtPlanLevel = mbPlan <&> (.subscribedFlagToggleAllowed)
        mbDriverStat <- QDS.findById (cast driverFee.driverId)
        case mbPlan of
          Nothing -> pure ()
          Just plan -> do
            let (planBaseFrequcency, baseAmount) = getFreqAndBaseAmountcase plan.planBaseAmount
                (mandateSetupDate, mandateId, waiveOffMode, waiveOffPercentage, waiveOffValidTill) = case mbDriverPlan of
                  Nothing -> (now, Nothing, DPlan.NO_WAIVE_OFF, 0.0, Nothing) -- if there is no driverplan in that case we pass Nothing to avoid waiveoff in getFinalOrderAmount function
                  Just driverPlan -> (fromMaybe now driverPlan.mandateSetupDate, driverPlan.mandateId, driverPlan.waiveOfMode, driverPlan.waiverOffPercentage, driverPlan.waiveOffValidTill)
                coinCashLeft = if plan.eligibleForCoinDiscount then max 0.0 $ maybe 0.0 (.coinCovertedToCashLeft) mbDriverStat else 0.0

            driver <- QP.findById (cast driverFee.driverId) >>= fromMaybeM (PersonDoesNotExist driverFee.driverId.getId)
            let numRidesForPlanCharges = calcNumRides driverFee transporterConfig - plan.freeRideCount

            --------- find all payment pending cancellation penalties ------------
            cancellationPenalties <- QDF.findPendingCancellationPenaltiesForDriver (cast driverFee.driverId) serviceName merchant.id merchantOpCityId
            let totalCancellationPenalty = sum $ map (\cp -> roundToHalf cp.currency $ fromMaybe 0 cp.cancellationPenaltyAmount) cancellationPenalties
            driverFeeWithPenalties <-
              if totalCancellationPenalty > 0
                then do
                  QDF.updateCancellationPenaltyAmount driverFee.id totalCancellationPenalty now
                  let cancellationPenaltyIds = map (.id) cancellationPenalties
                  QDF.updateStatusAndAddedToFeeId ADDED_TO_INVOICE (Just driverFee.id) cancellationPenaltyIds now
                  pure $ driverFee {cancellationPenaltyAmount = Just totalCancellationPenalty}
                else do
                  pure driverFee
            --------- calculations based of frequency happens here ------------
            (feeWithoutDiscount, totalFee, offerId, offerTitle) <- do
              calcFinalOrderAmounts merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges planBaseFrequcency baseAmount driverFeeWithPenalties waiveOffPercentage waiveOffMode waiveOffValidTill
            ---------------------------------------------------------------------
            ------------- update driver fee with offer and plan details ---------
            let offerAndPlanTitle = Just plan.name <> Just "-*@*-" <> offerTitle ---- this we will send in payment history ----
            updateOfferAndPlanDetails offerId offerAndPlanTitle (Just plan.id) (Just plan.paymentMode) driverFee.id
            let driverFeeUpdateWithPlanAndOffer =
                  driverFee
                    { offerId = offerId,
                      planOfferTitle = offerAndPlanTitle,
                      planId = Just plan.id,
                      planMode = Just plan.paymentMode,
                      cancellationPenaltyAmount = Just totalCancellationPenalty
                    }
            --------------------------------------------------
            let paymentMode = maybe MANUAL (.planType) mbDriverPlan
            let nonEmptyDriverId = NE.fromList [driverFee.driverId]
            ------------- process driver fee based on payment mode ----------------
            unless (totalFee == 0 && totalCancellationPenalty == 0) $ do
              -- driverFeeUpdateWithPlanAndOffer <- QDF.findById driverFee.id >>= fromMaybeM (InternalError $ "driverFee not found with driverFee id : " <> driverFee.id.getId)
              if coinCashLeft >= totalFee
                then do
                  void $ QDS.updateCoinToCashByDriverId (cast driverFeeUpdateWithPlanAndOffer.driverId) (-1.0 * totalFee)
                  setCoinToCashUsedAmount driverFeeUpdateWithPlanAndOffer totalFee
                  QDF.updateStatusByIds CLEARED_BY_YATRI_COINS [driverFeeUpdateWithPlanAndOffer.id] now
                  driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFeeUpdateWithPlanAndOffer mandateId Nothing subscriptionConfigs transporterConfig now
                  invoice <- mkInvoiceAgainstDriverFee driverFeeUpdateWithPlanAndOffer (True, paymentMode == AUTOPAY)
                  updateAmountPaidByCoins (Just totalFee) driverFeeUpdateWithPlanAndOffer.id
                  QINV.create invoice
                else do
                  when (coinCashLeft > 0) $ do
                    QDS.updateCoinToCashByDriverId (cast driverFeeUpdateWithPlanAndOffer.driverId) (-1.0 * coinCashLeft)
                    setCoinToCashUsedAmount driverFeeUpdateWithPlanAndOffer coinCashLeft
                  driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFeeUpdateWithPlanAndOffer mandateId (Just coinCashLeft) subscriptionConfigs transporterConfig now
                  updatePendingPayment True (cast driverFeeUpdateWithPlanAndOffer.driverId)
                  SLOSO.addSendOverlaySchedulerDriverIds merchantOpCityId (Just driverFee.vehicleCategory) (Just "PaymentOverdueGreaterThan") nonEmptyDriverId
                  SLOSO.addSendOverlaySchedulerDriverIds merchantOpCityId (Just driverFee.vehicleCategory) (Just "PaymentOverdueBetween") nonEmptyDriverId
            -------------------------------------------------------------------------------
            -- blocking
            dueDriverFees <- QDF.findAllFeeByTypeServiceStatusAndDriver serviceName (cast driverFee.driverId) [RECURRING_INVOICE, RECURRING_EXECUTION_INVOICE] [PAYMENT_PENDING, PAYMENT_OVERDUE]
            let driverFeeIds = map (.id) dueDriverFees
                due = sum $ map (\fee -> if (fee.startTime /= startTime && fee.endTime /= endTime) then roundToHalf driverFee.currency $ fee.govtCharges + fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst + fromMaybe 0 fee.cancellationPenaltyAmount else 0) dueDriverFees
            if roundToHalf driverFee.currency (due + totalFee - min coinCashLeft totalFee) >= fromMaybe plan.maxCreditLimit maxCreditLimitLinkedToDPlan
              then do
                mapM_ updateDriverFeeToManual driverFeeIds
                updateDriverFeeToManual driverFee.id
                when (fromMaybe plan.subscribedFlagToggleAllowed isPlanToggleAllowedAtPlanLevel) $ do
                  updateSubscription False (cast driverFee.driverId)
                  SLOSO.addSendOverlaySchedulerDriverIds merchantOpCityId (Just driverFee.vehicleCategory) (Just "BlockedDrivers") nonEmptyDriverId
              else do
                unless ((totalFee == 0 || coinCashLeft >= totalFee) && totalCancellationPenalty == 0) $ processDriverFee paymentMode driverFeeWithPenalties subscriptionConfigs transporterConfig
            updateSerialOrderForInvoicesInWindow driverFee.id merchantOpCityId startTime endTime serviceName

    case listToMaybe driverFees of
      Nothing -> do
        Hedis.del (mkDriverFeeBillNumberKey merchantOpCityId serviceName)
        duplicationKey <- Hedis.setNxExpire (jobDuplicationPreventionKey hashedJobData "DriverFeeCalc") (3600 * 12) True -- 12 hours
        when duplicationKey do
          scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId serviceName subscriptionConfigs jobData
        return Complete
      _ -> ReSchedule <$> getRescheduledTime (fromMaybe 5 transporterConfig.driverFeeCalculatorBatchGap)

  case lockResult of
    Left () -> do
      logError $ "Driver fee calculation job for merchant " <> merchantId.getId <> " could not acquire lock. Rescheduling job."
      ReSchedule <$> getRescheduledTime 600
    Right result -> return result

-- | Split cancellation penalty amount into separate DriverFees for AUTOPAY vendor routing
splitCancellationPenaltyIntoDriverFees ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  DriverFee ->
  SubscriptionConfig ->
  TransporterConfig ->
  UTCTime ->
  m ()
splitCancellationPenaltyIntoDriverFees parentDriverFee subscriptionConfig transporterConfig now = do
  let totalCancellationAmount = fromMaybe 0 parentDriverFee.cancellationPenaltyAmount
  when (totalCancellationAmount > 0) $ do
    mbDriverPlan <- QDPlan.findByDriverIdWithServiceName (cast parentDriverFee.driverId) parentDriverFee.serviceName
    mandate <- maybe (pure Nothing) QMD.findById (mbDriverPlan >>= (.mandateId))
    case mandate <&> (.maxAmount) of
      Nothing -> do
        logError $ "No mandate max amount found for driver plan " <> maybe "[No Plan]" (.getId) parentDriverFee.planId
        createCancellationPenaltyDriverFee parentDriverFee totalCancellationAmount (Just parentDriverFee.id) subscriptionConfig transporterConfig.cancellationFeeVendor now
      Just maxAmt -> do
        if totalCancellationAmount <= maxAmt || maxAmt == 0
          then do
            createCancellationPenaltyDriverFee parentDriverFee totalCancellationAmount (Just parentDriverFee.id) subscriptionConfig transporterConfig.cancellationFeeVendor now
          else do
            let numSplits = ceiling (totalCancellationAmount / maxAmt) :: Int
                amounts = splitAmountToMaximizeMandateUsage totalCancellationAmount maxAmt numSplits
            forM_ amounts $ \amount -> do
              createCancellationPenaltyDriverFee parentDriverFee amount (Just parentDriverFee.id) subscriptionConfig transporterConfig.cancellationFeeVendor now
    QDF.updateCancellationPenaltyAmount parentDriverFee.id 0 now
  where
    splitAmountToMaximizeMandateUsage :: HighPrecMoney -> HighPrecMoney -> Int -> [HighPrecMoney]
    splitAmountToMaximizeMandateUsage totalAmount maxAmount numSplits
      | numSplits <= 1 = [totalAmount]
      | otherwise =
        let fullSplits = replicate (numSplits - 1) maxAmount
            remainder = totalAmount - (fromIntegral (numSplits - 1) * maxAmount)
         in fullSplits ++ [remainder]

-- | Create a single CANCELLATION_PENALTY DriverFee with optional VendorFee
createCancellationPenaltyDriverFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  DriverFee ->
  HighPrecMoney ->
  Maybe (Id DriverFee) ->
  SubscriptionConfig ->
  Maybe Text ->
  UTCTime ->
  m ()
createCancellationPenaltyDriverFee parentFee amount mbSplitOfDriverFeeId subscriptionConfig vendor now = do
  childId <- generateGUID
  let childDriverFee =
        DriverFee
          { id = childId,
            govtCharges = 0,
            platformFee = PlatformFee {fee = 0, cgst = 0, sgst = 0, currency = parentFee.currency},
            driverId = parentFee.driverId,
            merchantId = parentFee.merchantId,
            merchantOperatingCityId = parentFee.merchantOperatingCityId,
            serviceName = parentFee.serviceName,
            cancellationPenaltyAmount = Just amount,
            feeType = RECURRING_EXECUTION_INVOICE,
            splitOfDriverFeeId = mbSplitOfDriverFeeId,
            status = PAYMENT_PENDING,
            autopayPaymentStage = Just NOTIFICATION_SCHEDULED,
            createdAt = now,
            updatedAt = now,
            numRides = 0,
            addedToFeeId = Nothing,
            amountPaidByCoin = Nothing,
            badDebtDeclarationDate = Nothing,
            badDebtRecoveryDate = Nothing,
            billNumber = Nothing,
            collectedAt = Nothing,
            collectedBy = Nothing,
            currency = parentFee.currency,
            endTime = parentFee.endTime,
            feeWithoutDiscount = Nothing,
            hasSibling = Nothing,
            notificationRetryCount = parentFee.notificationRetryCount,
            offerId = Nothing,
            overlaySent = False,
            payBy = parentFee.payBy,
            planId = parentFee.planId,
            planMode = parentFee.planMode,
            planOfferTitle = parentFee.planOfferTitle,
            refundEntityId = Nothing,
            refundedAmount = Nothing,
            refundedAt = Nothing,
            refundedBy = Nothing,
            schedulerTryCount = parentFee.schedulerTryCount,
            siblingFeeId = Nothing,
            specialZoneAmount = 0,
            specialZoneRideCount = 0,
            stageUpdatedAt = Nothing,
            startTime = parentFee.startTime,
            totalEarnings = 0,
            validDays = parentFee.validDays,
            vehicleCategory = parentFee.vehicleCategory,
            vehicleNumber = Nothing
          }
  QDF.create childDriverFee
  when (fromMaybe False subscriptionConfig.isVendorSplitEnabled && isJust vendor) $ do
    let vendorFee =
          DVF.VendorFee
            { driverFeeId = childId,
              vendorId = fromMaybe "CANCELLATION_PENALTY_VENDOR" vendor,
              amount = amount,
              createdAt = now,
              updatedAt = now
            }
    QVF.create vendorFee

  invoice <- mkInvoiceAgainstDriverFee childDriverFee (False, True)
  QINV.create invoice
  QDF.updateAutopayPaymentStageById (Just NOTIFICATION_SCHEDULED) (Just now) childId

processDriverFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  PaymentMode ->
  DriverFee ->
  SubscriptionConfig ->
  TransporterConfig ->
  m ()
processDriverFee paymentMode driverFee subscriptionConfig transporterConfig = do
  now <- getCurrentTime
  case paymentMode of
    MANUAL -> do
      _ <- withTryCatch "makeVendorFeeForCancellationPenalty:processDriverFee" $ makeVendorFeeForCancellationPenalty driverFee subscriptionConfig transporterConfig
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
      _ <- withTryCatch "splitCancellationPenaltyIntoDriverFees:processDriverFee" $ splitCancellationPenaltyIntoDriverFees driverFee subscriptionConfig transporterConfig now
      updateStatus PAYMENT_PENDING driverFee.id now
      updateFeeType RECURRING_EXECUTION_INVOICE driverFee.id
      invoice <- mkInvoiceAgainstDriverFee driverFee (False, True)
      QINV.create invoice
      QDF.updateAutopayPaymentStageById (Just NOTIFICATION_SCHEDULED) (Just now) driverFee.id

processRestFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  PaymentMode ->
  DriverFee ->
  [DVF.VendorFee] ->
  SubscriptionConfig ->
  DriverFee ->
  HighPrecMoney ->
  TransporterConfig ->
  m ()
processRestFee paymentMode DriverFee {..} vendorFees subscriptionConfig _ _ transporterConfig = do
  let driverFee =
        DriverFee
          { status = if paymentMode == MANUAL && not (subscriptionConfig.allowManualPaymentLinks) then PAYMENT_OVERDUE else PAYMENT_PENDING,
            feeType = if paymentMode == MANUAL then RECURRING_INVOICE else RECURRING_EXECUTION_INVOICE,
            ..
          }
  QDF.create driverFee
  when (fromMaybe False subscriptionConfig.isVendorSplitEnabled) $ mapM_ (QVF.create) vendorFees
  processDriverFee paymentMode driverFee subscriptionConfig transporterConfig
  updateSerialOrderForInvoicesInWindow driverFee.id merchantOperatingCityId startTime endTime driverFee.serviceName

makeOfferReq :: HighPrecMoney -> Person -> Plan -> UTCTime -> UTCTime -> Int -> TransporterConfig -> Payment.OfferListReq
makeOfferReq totalFee driver plan dutyDate registrationDate numOfRides transporterConfig = do
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = totalFee, currency = transporterConfig.currency} -- add UDFs
      customerReq = Payment.OfferCustomer {customerId = driver.id.getId, email = driver.email, mobile = Nothing}
  Payment.OfferListReq
    { order = offerOrder,
      customer = Just customerReq,
      planId = plan.id.getId,
      registrationDate,
      paymentMode = getPaymentModeAndVehicleCategoryKey plan,
      dutyDate,
      numOfRides,
      offerListingMetric = if transporterConfig.enableUdfForOffers then Just Payment.IS_APPLICABLE else Nothing
    }

getFinalOrderAmount ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  HighPrecMoney ->
  Id Merchant ->
  TransporterConfig ->
  Person ->
  Plan ->
  UTCTime ->
  Int ->
  DriverFee ->
  HighPrecMoney ->
  DPlan.WaiveOffMode ->
  Maybe UTCTime ->
  m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan registrationDate numOfRidesConsideredForCharges driverFee waiveOffPercentage waiveOffMode waiveOffValidTill = do
  now <- getCurrentTime
  let dutyDate = driverFee.createdAt
      registrationDateLocal = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) registrationDate
      waiveOffValidTillIst = fmap (addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc)) waiveOffValidTill
      waiveOffMultiplier = if (waiveOffMode == DPlan.NO_WAIVE_OFF || maybe True (driverFee.startTime >) waiveOffValidTillIst) then 1.0 else (1.0 - (waiveOffPercentage / 100)) -- If there is no driver plan or waive-off validity is Nothing, no discount applies
      feeWithoutDiscountWithWaiveOff = feeWithoutDiscount * waiveOffMultiplier
      feeWithoutDiscountWithWaiveOffAndSpecialZone = feeWithoutDiscountWithWaiveOff + driverFee.specialZoneAmount
      feeWithOutDiscountPlusSpecialZone = feeWithoutDiscount + driverFee.specialZoneAmount
  if (feeWithOutDiscountPlusSpecialZone == 0 || feeWithoutDiscountWithWaiveOffAndSpecialZone == 0)
    then do
      updateCollectedPaymentStatus CLEARED Nothing now driverFee.id
      return (0, 0, Nothing, Nothing)
    else do
      offerResp <- do
        case waiveOffMode of
          DPlan.WITHOUT_OFFER -> return []
          _ -> do
            offers <- SPayment.offerListCache merchantId driverFee.driverId driverFee.merchantOperatingCityId plan.serviceName (makeOfferReq feeWithoutDiscountWithWaiveOff driver plan dutyDate registrationDateLocal numOfRidesConsideredForCharges transporterConfig) -- handle UDFs
            return offers.offerResp
      (finalOrderAmount, offerId, offerTitle) <-
        if null offerResp
          then pure (feeWithoutDiscountWithWaiveOff, Nothing, Nothing)
          else do
            let bestOffer = minimumBy (comparing (.finalOrderAmount)) offerResp
            pure (bestOffer.finalOrderAmount, Just bestOffer.offerId, bestOffer.offerDescription.title)
      if finalOrderAmount + driverFee.specialZoneAmount == 0
        then do
          updateCollectedPaymentStatus CLEARED offerId now driverFee.id
          updateFeeWithoutDiscount (Just feeWithOutDiscountPlusSpecialZone) driverFee.id
          return (0, 0, offerId, offerTitle)
        else return (feeWithOutDiscountPlusSpecialZone, finalOrderAmount + driverFee.specialZoneAmount, offerId, offerTitle)

splitPlatformFee :: (MonadFlow m) => HighPrecMoney -> HighPrecMoney -> Plan -> DriverFee -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> [DVF.VendorFee] -> UTCTime -> m [(DriverFee, [DVF.VendorFee])]
splitPlatformFee feeWithoutDiscount_ totalFee plan DriverFee {..} maxAmountPerDriverfeeThreshold coinClearedAmount vendorFees now = do
  let maxAmount = fromMaybe totalFee maxAmountPerDriverfeeThreshold
  let numEntities = totalFee / maxAmount
      numEntitiesInt = floor numEntities :: Integer
      remainingFee = totalFee `mod'` maxAmount
      coinDiscount = if remainingFee <= 0 then coinClearedAmount else Nothing
      vendorFeeAmountEqualPartsAndRemaining =
        map
          ( \vf -> do
              let amountVf = vf.amount / numEntities
              let remainingVf = vf.amount - (amountVf * (HighPrecMoney $ toRational numEntitiesInt))
              let vendorId = vf.vendorId
              (amountVf, vendorId, remainingVf)
          )
          $ vendorFees
  newIds <- replicateM (fromInteger $ if remainingFee == 0.0 then numEntitiesInt - 1 else numEntitiesInt) (generateGUID)
  let idsToApply = newIds <> [id]
  let vendorFeeAmountEqualParts :: [(HighPrecMoney, Text)] = map (\(amount, vendorId, _) -> (amount, vendorId)) vendorFeeAmountEqualPartsAndRemaining
  let vendorFeeAmountRemaining :: [(HighPrecMoney, Text)] = map (\(_, vendorId, amount) -> (amount, vendorId)) vendorFeeAmountEqualPartsAndRemaining
  let entityList = replicate (fromInteger numEntitiesInt) (maxAmount, coinDiscount, vendorFeeAmountEqualParts) ++ [(remainingFee, coinClearedAmount, vendorFeeAmountRemaining) | remainingFee > 0]
  let entityListZpWithId = zip idsToApply entityList
   in mapM
        ( \(driverFeeId, (fee, coinPaidAmount, vendorFeeData)) -> do
            let (platformFee_, cgst, sgst) = calculatePlatformFeeAttr fee plan
                dfee =
                  DriverFee
                    { platformFee = PlatformFee {fee = platformFee_, ..},
                      feeType = feeType,
                      feeWithoutDiscount = Just feeWithoutDiscount_, -- same for all splitted ones, not remaining fee
                      amountPaidByCoin = coinPaidAmount,
                      splitOfDriverFeeId = if driverFeeId /= id then Just id else Nothing,
                      id = driverFeeId,
                      ..
                    }
            let vfs = map (\(amount, vendorId) -> DVF.VendorFee {driverFeeId = driverFeeId, vendorId = vendorId, amount = amount, createdAt = now, updatedAt = now}) $ vendorFeeData
            return $ (dfee, vfs)
        )
        $ entityListZpWithId

getFreqAndBaseAmountcase :: PlanBaseAmount -> (Text, HighPrecMoney)
getFreqAndBaseAmountcase planBaseAmount = case planBaseAmount of
  PERRIDE_BASE amount -> ("PER_RIDE" :: Text, amount)
  DAILY_BASE amount -> ("DAILY" :: Text, amount)
  WEEKLY_BASE amount -> ("WEEKLY" :: Text, amount)
  MONTHLY_BASE amount -> ("MONTHLY" :: Text, amount)
  RECHARGE_BASE amount -> ("RECHARGE" :: Text, amount)

driverFeeSplitter ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  PaymentMode ->
  Plan ->
  HighPrecMoney ->
  HighPrecMoney ->
  DriverFee ->
  Maybe (Id Mandate) ->
  Maybe HighPrecMoney ->
  SubscriptionConfig ->
  TransporterConfig ->
  UTCTime ->
  m ()
driverFeeSplitter paymentMode plan feeWithoutDiscount totalFee driverFee mandateId mbCoinAmountUsed subscriptionConfigs transporterConfig now = do
  mandate <- maybe (pure Nothing) QMD.findById mandateId
  let amountForSpiltting = if isNothing mbCoinAmountUsed then Just $ roundToHalf driverFee.currency totalFee else roundToHalf driverFee.currency <$> (mandate <&> (.maxAmount))
      coinAmountUsed = fromMaybe 0 mbCoinAmountUsed
      totalFeeWithCoinDeduction = roundToHalf driverFee.currency $ totalFee - coinAmountUsed
  vendorFees <- QVF.findAllByDriverFeeId driverFee.id
  splittedFeesWithRespectiveVendorFee <- splitPlatformFee feeWithoutDiscount totalFeeWithCoinDeduction plan driverFee amountForSpiltting mbCoinAmountUsed vendorFees now
  case splittedFeesWithRespectiveVendorFee of
    [] -> throwError (InternalError "No driver fee entity with non zero total fee")
    _ -> do
      forM_ splittedFeesWithRespectiveVendorFee $ \(dfee, vfee) -> do
        if dfee.id /= driverFee.id
          then processRestFee paymentMode dfee vfee subscriptionConfigs driverFee totalFee transporterConfig
          else do
            -- Reset The Original Fee Amount & adjust the vendor fee amount by subtracting sums of child vendor fees
            resetFee dfee.id dfee.govtCharges dfee.platformFee (Just feeWithoutDiscount) dfee.amountPaidByCoin now
            QVF.resetVendorFee dfee.merchantOperatingCityId vfee

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
  Bool ->
  SubscriptionConfig ->
  m [DriverFee]
getOrGenerateDriverFeeDataBasedOnServiceName serviceName startTime endTime merchantId merchantOperatingCityId transporterConfig recalculateManualReview subsConfig = do
  now <- getCurrentTime
  let statusToCheck = if recalculateManualReview then MANUAL_REVIEW_NEEDED else ONGOING
  let enableCityBasedFeeSwitch = subsConfig.enableCityBasedFeeSwitch
  case serviceName of
    YATRI_SUBSCRIPTION -> do
      driverFeeElderSiblings <- QDF.findAllFeesInRangeWithStatusAndServiceName (Just merchantId) merchantOperatingCityId startTime endTime statusToCheck transporterConfig.driverFeeCalculatorBatchSize serviceName enableCityBasedFeeSwitch
      --- here we are only finding siblings of that particular city due to disablement of city based fee switch we need to do nub as duplicate entries can be there ----
      driverFeeRestSiblings <- QDF.findAllChildsOFDriverFee merchantOperatingCityId startTime endTime statusToCheck serviceName (map (.id) $ filter (\dfee -> dfee.hasSibling == Just True) driverFeeElderSiblings) True
      return $ filter (\dfee -> dfee.merchantOperatingCityId == merchantOperatingCityId) $ nubBy (\x y -> x.id == y.id) $ driverFeeElderSiblings <> driverFeeRestSiblings
    YATRI_RENTAL -> generateDriverFee now enableCityBasedFeeSwitch
    DASHCAM_RENTAL _ -> generateDriverFee now enableCityBasedFeeSwitch
    PREPAID_SUBSCRIPTION -> pure []
  where
    generateDriverFee now enableCityBasedFeeSwitch = do
      when (startTime >= endTime) $ throwError (InternalError "Invalid time range for driver fee calculation")
      let mbStartTime = Just startTime
          mbEndTime = Just endTime
      mbtoCreateDriverFeeForService <- toCreateDriverFeeForService serviceName merchantOperatingCityId
      if fromMaybe True mbtoCreateDriverFeeForService
        then do
          driverEligibleForRentals <- findAllDriversEligibleForService serviceName merchantId merchantOperatingCityId endTime (fromMaybe 0 transporterConfig.driverFeeCalculatorBatchSize)
          driverFees <-
            mapMaybeM
              ( \dPlan -> do
                  mbExistingDFee <- QDF.findFeeByDriverIdAndServiceNameInRange (cast dPlan.driverId) serviceName startTime endTime
                  if isNothing mbExistingDFee
                    then do
                      currency <- SMerchant.getCurrencyByMerchantOpCity merchantOperatingCityId
                      driverFee <- mkDriverFee serviceName now mbStartTime mbEndTime merchantId dPlan.driverId Nothing 0 0.0 0.0 0.0 currency transporterConfig Nothing False Nothing (Just subsConfig)
                      updateLastBillGeneratedAt (cast dPlan.driverId) serviceName endTime
                      QDF.create driverFee
                      return $ Just driverFee
                    else return Nothing
              )
              driverEligibleForRentals
          setCreateDriverFeeForServiceInSchedulerKey serviceName merchantOperatingCityId False
          return $ maybe driverFees (\limit -> take limit driverFees) transporterConfig.driverFeeCalculatorBatchSize
        else QDF.findAllFeesInRangeWithStatusAndServiceName (Just merchantId) merchantOperatingCityId startTime endTime ONGOING transporterConfig.driverFeeCalculatorBatchSize serviceName enableCityBasedFeeSwitch

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
        merchantId = Just driverFee.merchantId,
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
  ServiceNames ->
  SubscriptionConfig ->
  CalculateDriverFeesJobData ->
  m ()
scheduleJobs transporterConfig startTime endTime merchantId merchantOpCityId serviceName subscriptionConfigs jobData = do
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let dfNotificationTime = transporterConfig.driverAutoPayNotificationTime
  let timeDiffNormalFlow = addUTCTime dfNotificationTime endTime
  let timeDiffNormalFlowLinkSendJob = addUTCTime subscriptionConfigs.paymentLinkJobTime endTime
  let dfCalculationJobTs = if timeDiffNormalFlow > now then diffUTCTime timeDiffNormalFlow now else 5 * 60 --- 5 min
  let paymentLinkSendJobTs = if timeDiffNormalFlowLinkSendJob > now then diffUTCTime timeDiffNormalFlowLinkSendJob now else 5 * 60 --- 5 min
      scheduleChildJobs = fromMaybe True jobData.createChildJobs && not (fromMaybe False jobData.recalculateManualReview)
      scheduleNotification = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleNotification
      scheduleOverlay = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleOverlay
      scheduleManualPaymentLink = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleManualPaymentLink
      scheduleDriverFeeCalc = scheduleChildJobs && fromMaybe scheduleChildJobs jobData.scheduleDriverFeeCalc
  when scheduleNotification $ do
    createJobIn @_ @'SendPDNNotificationToDriver (Just merchantId) (Just merchantOpCityId) dfCalculationJobTs $
      SendPDNNotificationToDriverJobData
        { merchantId = merchantId,
          merchantOperatingCityId = Just merchantOpCityId,
          startTime = startTime,
          endTime = endTime,
          retryCount = Just 0,
          serviceName = Just serviceName
        }
  when (subscriptionConfigs.useOverlayService && scheduleOverlay) $ do
    createJobIn @_ @'SendOverlay (Just merchantId) (Just merchantOpCityId) (dfCalculationJobTs + 5400) $
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
          serviceName = Just serviceName,
          vehicleCategory = Nothing
        }
    createJobIn @_ @'SendOverlay (Just merchantId) (Just merchantOpCityId) (dfCalculationJobTs + 5400) $
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
          serviceName = Just serviceName,
          vehicleCategory = Nothing
        }
  when (subscriptionConfigs.allowManualPaymentLinks && scheduleManualPaymentLink) $ do
    createJobIn @_ @'SendManualPaymentLink (Just merchantId) (Just merchantOpCityId) paymentLinkSendJobTs $
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
            createJobIn @_ @'CalculateDriverFees (Just merchantId) (Just merchantOpCityId) dfCalculationJobTs' $
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
                  recalculateManualReview = Nothing,
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
    EncFlow m r,
    HasKafkaProducer r
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
  HighPrecMoney ->
  DPlan.WaiveOffMode ->
  Maybe UTCTime ->
  m (HighPrecMoney, HighPrecMoney, Maybe Text, Maybe Text)
calcFinalOrderAmounts merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges planBaseFrequcency baseAmount driverFee waiveOffPercentage waiveOffMode waiveOffValidTill =
  case (planBaseFrequcency, plan.basedOnEntity) of
    ("PER_RIDE", RIDE) -> do
      let feeWithoutDiscount = max 0 (min plan.maxAmount (baseAmount * HighPrecMoney (toRational numRidesForPlanCharges)))
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee waiveOffPercentage waiveOffMode waiveOffValidTill
    ("DAILY", RIDE) -> do
      let feeWithoutDiscount = if numRidesForPlanCharges > 0 then baseAmount else 0
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee waiveOffPercentage waiveOffMode waiveOffValidTill
    ("DAILY", NONE) -> do
      let feeWithoutDiscount = baseAmount
      getFinalOrderAmount feeWithoutDiscount merchantId transporterConfig driver plan mandateSetupDate numRidesForPlanCharges driverFee waiveOffPercentage waiveOffMode waiveOffValidTill
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
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasKafkaProducer r
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
  transporterConfig <- SCTC.findByMerchantOpCityId opCityId Nothing >>= fromMaybeM (TransporterConfigNotFound opCityId.getId)
  subscriptionConfigs <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId Nothing serviceName >>= fromMaybeM (InternalError $ "No subscription config found" <> show serviceName)
  -- Validate that the time difference since endTime does not exceed the max delay threshold
  let nowLocalTime = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now
      timeDiffSinceEndTime = diffUTCTime nowLocalTime endTime
  case subscriptionConfigs.sendManualPaymentLinkJobMaxDelay of
    Just maxDelay
      | timeDiffSinceEndTime > maxDelay ->
        return $ Terminate $ "Job scheduled too late. Time since endTime: " <> show (timeDiffSinceEndTime / 3600) <> " hours, Maximum allowed delay: " <> show (maxDelay / 3600) <> " hours. Marking as Failed."
    _ -> do
      let deepLinkExpiry = subscriptionConfigs.deepLinkExpiryTimeInMinutes
      driverPlansToProccess <- findAllDriversToSendManualPaymentLinkWithLimit serviceName merchantId opCityId endTime subscriptionConfigs.genericBatchSizeForJobs
      if null driverPlansToProccess
        then return Complete
        else do
          processAndSendManualPaymentLink driverPlansToProccess subscriptionConfigs merchantId opCityId serviceName deepLinkExpiry endTime now
          ReSchedule <$> getRescheduledTime subscriptionConfigs.genericJobRescheduleTime

processAndSendManualPaymentLink ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig, HasKafkaProducer r) =>
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
    -- Rate limiting check: max subscriptionConfigs.maxRetryCount messages per driver within configured expiry window
    let rateLimitKey = mkPaymentLinkRateLimitKey driverId
        expirySeconds = maybe 21600 (getSeconds . nominalDiffTimeToSeconds) subscriptionConfigs.manualPaymentLinkRateLimitExpirySeconds
    currentCount <- incrWithExpiry rateLimitKey expirySeconds
    let withinRateLimit = currentCount <= fromIntegral subscriptionConfigs.maxRetryCount
    unless withinRateLimit $ logError $ "Rate limit exceeded for driver: " <> driverId.getId <> " (count: " <> show currentCount <> "). Skipping payment link send."
    when withinRateLimit $ do
      getPendingAndOverDueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [PAYMENT_OVERDUE, PAYMENT_PENDING] Nothing serviceName
      updateStatusByIds PAYMENT_OVERDUE (map (.id) $ filter ((== PAYMENT_PENDING) . (.status)) getPendingAndOverDueDriverFees) now
      let allowDeepLink = subscriptionConfigs.sendDeepLink
      let mbDeepLinkData = if allowDeepLink then Just $ SPayment.DeepLinkData {sendDeepLink = Just True, expiryTimeInMinutes = mbDeepLinkExpiry} else Nothing --Nothing
      if not $ null getPendingAndOverDueDriverFees
        then do
          resp' <- withTryCatch "clearDriverDues:processAndSendManualPaymentLink" $ DDriver.clearDriverDues (driverId, merchantId, opCityId) serviceName Nothing mbDeepLinkData
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
                whatsAppResp <- withTryCatch "sendLinkTroughChannelProvided:processAndSendManualPaymentLink" $ SPayment.sendLinkTroughChannelProvided mbPaymentLink driverId mbAmount (Just subscriptionConfigs.paymentLinkChannel) allowDeepLink WHATSAPP_SEND_MANUAL_PAYMENT_LINK
                errorCatchAndHandle driverId whatsAppResp subscriptionConfigs endTime now (\_ -> QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName (Just endTime) driverId serviceName)
            )
        else do
          QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName (Just endTime) driverId serviceName

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
      let expirySeconds = maybe 21600 (getSeconds . nominalDiffTimeToSeconds) subscriptionConfig.manualPaymentLinkRateLimitExpirySeconds
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkManualLinkErrorTrackingByDriverIdKey driverId) subscriptionConfig.maxRetryCount expirySeconds
      if eligibleForRetryInNextBatch
        then return ()
        else QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName (Just endTime) driverId subscriptionConfig.serviceName
    Right resp -> function resp

-- | Increment a Redis key and ensure it has an expiry set
-- Checks TTL after increment and sets expiry if missing (TTL = -1)
-- This prevents keys from persisting indefinitely if process crashes between INCR and EXPIRE
incrWithExpiry :: (MonadFlow m, CacheFlow m r) => Text -> Int -> m Integer
incrWithExpiry key expirySeconds = do
  count <- Hedis.incr key
  ttl <- Hedis.ttl key
  when (ttl == -1) $ Hedis.expire key expirySeconds
  return count

isEligibleForRetryInNextBatch :: (MonadFlow m, CacheFlow m r) => Text -> Int -> Int -> m Bool
isEligibleForRetryInNextBatch key maxCount expirySeconds = do
  count <- incrWithExpiry key expirySeconds
  return $ count <= fromIntegral maxCount

mkManualLinkErrorTrackingByDriverIdKey :: Id Driver -> Text
mkManualLinkErrorTrackingByDriverIdKey driverId = "ErrorRetryCountFor:DriverId:" <> driverId.getId

mkPaymentLinkRateLimitKey :: Id Driver -> Text
mkPaymentLinkRateLimitKey driverId = "SendPaymentLink:RateLimit:DriverId:" <> driverId.getId

getsetManualLinkErrorTrackingKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Maybe Int)
getsetManualLinkErrorTrackingKey driverId = Hedis.get (mkManualLinkErrorTrackingByDriverIdKey driverId)

makeVendorFeeForCancellationPenalty ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) =>
  DriverFee ->
  SubscriptionConfig ->
  TransporterConfig ->
  m ()
makeVendorFeeForCancellationPenalty driverFee subscriptionConfig transporterConfig = when (fromMaybe False subscriptionConfig.isVendorSplitEnabled && isJust transporterConfig.cancellationFeeVendor && fromMaybe 0 driverFee.cancellationPenaltyAmount > 0) $ do
  let vendorFee =
        DVF.VendorFee
          { driverFeeId = driverFee.id,
            vendorId = fromMaybe "CANCELLATION_PENALTY_VENDOR" transporterConfig.cancellationFeeVendor,
            amount = fromMaybe 0 driverFee.cancellationPenaltyAmount,
            createdAt = driverFee.createdAt,
            updatedAt = driverFee.updatedAt
          }
  QVF.create vendorFee

updateCancellationPenaltyAccumulationFees :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasKafkaProducer r) => ServiceNames -> TransporterConfig -> Id Merchant -> Id MerchantOperatingCity -> m ()
updateCancellationPenaltyAccumulationFees serviceName transporterConfig merchantId merchantOperatingCityId = do
  now <- getCurrentTime
  QDF.moveCancellationPenaltiesToIndisputeWindow serviceName merchantId merchantOperatingCityId
  let disputeWindowSeconds = secondsToNominalDiffTime $ fromMaybe (Seconds 172800) transporterConfig.cancellationFeeDisputeWindow
  disputePenalties <- QDF.findAllCancellationPenaltiesInDisputeWindow serviceName merchantId merchantOperatingCityId
  forM_ disputePenalties $ \penalty -> do
    let disputeEndTime = addUTCTime disputeWindowSeconds penalty.endTime
    when (now >= disputeEndTime) $ do
      let penaltyAmount = fromMaybe 0.0 penalty.cancellationPenaltyAmount
          amountWithGst = penaltyAmount * 1.18
      QDF.moveCancellationPenaltyToPaymentPending penalty.id amountWithGst
  logInfo $ "updateCancellationPenaltyAccumulationFees: Processed cancellation penalty status changes for service: " <> show serviceName <> " merchant: " <> merchantId.getId <> " operatingCity: " <> merchantOperatingCityId.getId
