{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.EndRide.Internal
  ( endRideTransaction,
    createDriverWalletTransaction,
    processEndRideFinance,
    putDiffMetric,
    getRouteAndDistanceBetweenPoints,
    safeMod,
    getCurrentDate,
    getRidesAndDistancefromZscore,
    getRouteInfoWithShortestDuration,
    mkDriverFeeCalcJobFlagKey,
    getDriverFeeCalcJobFlagKey,
    getPlan,
    pickWaypoints,
    getDriverFeeBillNumberKey,
    mkDriverFeeBillNumberKey,
    mkDriverFee,
    getDriverFeeCalcJobCache,
    setDriverFeeCalcJobCache,
    makeDriverLeaderBoardKey,
    getMonth,
    pickedWaypointsForEditDestination,
    pickNWayPoints,
    makeWalletRunningBalanceLockKey,
    makeSubscriptionRunningBalanceLockKey,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Action.Dashboard.Common as DCommon
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.SharedLogic.RideDiscount as RD
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationCharges as DCC
import qualified Domain.Types.CancellationDuesDetails as DCDD
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.FarePolicy as DFP
import "beckn-spec" Domain.Types.Invoice (IssuedToType (..))
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.SubscriptionConfig as DSC
import qualified Domain.Types.SubscriptionPurchase as DSP
import Domain.Types.TransporterConfig hiding (InvoiceConfig)
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as Variant
import qualified Domain.Types.VendorFee as DVF
import qualified Domain.Types.VendorSplitDetails as DVSD
import qualified Environment
import EulerHS.Prelude hiding (elem, foldr, id, length, map, mapM_, null)
import GHC.Float (double2Int)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude hiding (find, forM_, map, whenJust)
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Finance (AccountRole (..), InvoiceConfig (..), InvoiceLineItem (..), ItemType (..), LineItemDescription (..), invoice, runFinance, transfer, transferWithoutAttribution, transfer_)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import Lib.Types.SpecialLocation hiding (Merchant, MerchantOperatingCity)
import qualified SharedLogic.ActiveDriversList as ADL
import qualified SharedLogic.AirportEntryFee as AirportEntryFee
import SharedLogic.Allocator
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import SharedLogic.DriverFee (calculatePlatformFeeAttr)
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator
import qualified SharedLogic.FareCalculator as FC
import SharedLogic.FarePolicy
import SharedLogic.Finance.Prepaid
import SharedLogic.Finance.Wallet
import SharedLogic.Ride (makeSubscriptionRunningBalanceLockKey, multipleRouteKey, searchRequestKey, updateOnRideStatusWithAdvancedRideCheck)
import qualified SharedLogic.RideEvents.Publisher as RideEventsPublisher
import Storage.Beam.Toll ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.CachedQueries.VendorSplitDetails as CQVSD
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import Storage.Queries.DriverPlan (findByDriverIdWithServiceName)
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.FleetDriverAssociationExtra as QFDAE
import Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSPE
import qualified Storage.Queries.VendorFee as QVF
import Toll.SharedLogic.TollsDetector
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications
import qualified Tools.PaymentNudge as PaymentNudge
import Tools.Utils

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    ClickhouseFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text],
    HasField "activeDriversListKeyShards" r Int,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "rideEventsPublisherCfg" r (Maybe Environment.RideEventsPublisherCfg),
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    CoreMetrics m
  ) =>
  Id DP.Driver ->
  SRB.Booking ->
  Ride.Ride ->
  Maybe DFare.FareParameters ->
  Maybe (Id RD.RiderDetails) ->
  DFare.FareParameters ->
  TransporterConfig ->
  m ()
endRideTransaction driverId booking ride mbFareParams mbRiderDetailsId newFareParams thresholdConfig = do
  updateOnRideStatusWithAdvancedRideCheck ride.driverId (Just ride)
  oldDriverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let newFlowStatus = DDriverMode.getDriverFlowStatus oldDriverInfo.mode oldDriverInfo.active
  DDriverMode.updateDriverModeAndFlowStatus driverId thresholdConfig oldDriverInfo.active Nothing newFlowStatus oldDriverInfo (Just False) Nothing
  let driverInfo = oldDriverInfo {DI.driverFlowStatus = Just newFlowStatus}
  whenJust mbRiderDetailsId $ \riderDetailsId -> do
    QRiderDetails.updateCompletedRidesCount riderDetailsId.getId
  whenJust mbFareParams QFare.create
  QRB.updateStatus booking.id SRB.COMPLETED
  QRide.updateAll ride.id ride
  let safetyPlusCharges = maybe Nothing (\a -> find (\ac -> ac.chargeCategory == DAC.SAFETY_PLUS_CHARGES) a) $ (mbFareParams <&> (.conditionalCharges)) <|> (Just newFareParams.conditionalCharges)
  -- driverRideCount is incremented here for stats persistence; RC-stats-reminders and
  -- fleet/operator-dashboard side-effects now run in kafka-consumers RIDE_EVENTS_CONSUMER.
  void $ QDriverStats.incrementTotalRidesAndTotalDistAndIdleTime (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)
  when (isJust safetyPlusCharges) $ QDriverStats.incSafetyPlusRiderCountAndEarnings (cast ride.driverId) (fromMaybe 0.0 $ safetyPlusCharges <&> (.charge))
  Hedis.del $ multipleRouteKey booking.transactionId
  Hedis.del $ searchRequestKey booking.transactionId
  clearCachedFarePolicyByEstOrQuoteId booking.quoteId
  clearTollStartGateBatchCache ride.driverId.getId
  mbRiderDetails <- join <$> QRD.findById `mapM` mbRiderDetailsId
  let currency = booking.currency
  let customerCancellationDues = fromMaybe 0.0 newFareParams.customerCancellationDues
  logInfo $ "customerCancellationDues: newFareParams.customerCancellationDues: " <> show customerCancellationDues <> " ride.id: " <> show ride.id.getId <> " thresholdConfig.canAddCancellationFee: " <> show thresholdConfig.canAddCancellationFee
  let cancellationDues = fromMaybe 0.0 booking.fareParams.customerCancellationDues
  logInfo $ "cancellationDues: booking cancellationDues: " <> show cancellationDues <> " ride.id: " <> show ride.id.getId <> " thresholdConfig.canAddCancellationFee: " <> show thresholdConfig.canAddCancellationFee
  when (thresholdConfig.canAddCancellationFee && cancellationDues > 0.0) $ do
    case mbRiderDetails of
      Just riderDetails -> do
        id <- generateGUID
        let cancellationCharges =
              DCC.CancellationCharges
                { driverId = cast driverId,
                  rideId = Just ride.id,
                  cancellationCharges = cancellationDues,
                  ..
                }
        -- calDisputeChances <-
        --   if thresholdConfig.cancellationFee == 0.0
        --     then do
        --       logWarning "Unable to calculate dispute chances used"
        --       return 0
        --     else do
        --       return $ round (customerCancellationDues / thresholdConfig.cancellationFee)
        QRD.updateCancellationDuesPaid cancellationDues riderDetails.id.getId
        QRD.updateNoOfTimesCanellationDuesPaid riderDetails.id.getId
        QRD.updateCancellationDues 0 riderDetails.id >> QCC.create cancellationCharges
        pendingDues <- QCDD.findAllPendingByRiderId riderDetails.id
        let pendingIds = (.id) <$> pendingDues
            bppRideIds = (\d -> d.rideId.getId) <$> pendingDues
        unless (null pendingIds) $ do
          QCDD.updateStatusByIds DCDD.PAID pendingIds
          appBackendBapInternal <- asks (.appBackendBapInternal)
          fork "updateCancellationFeeStatusOnBAP" $ do
            void $
              CallBAPInternal.updateCancellationFeeStatus
                appBackendBapInternal.apiKey
                appBackendBapInternal.url
                (CallBAPInternal.UpdateCancellationFeeStatusReq {bppRideIds = bppRideIds})
      -- QRD.updateDisputeChancesUsedAndCancellationDues (max 0 (riderDetails.disputeChancesUsed - calDisputeChances)) 0 (riderDetails.id) >> QCC.create cancellationCharges
      _ -> logWarning $ "Unable to update customer cancellation dues as RiderDetailsId is NULL with rideId " <> ride.id.getId
  merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)

  fork "processEndRideFinance" $ processEndRideFinance merchant ride booking newFareParams driverId driverInfo thresholdConfig

  let validRide = isValidRide ride
  -- Publish RideEndedEvent to Redis Stream "ride.events.shard<N>". kafka-consumers
  -- RIDE_EVENTS_CONSUMER fans this out to: analytics Kafka events, ride-interpolation,
  -- namma tags, fleet/operator stats, GPS-toll-behavior, RC stats reminders, ride-end
  -- notifications, and leaderboard updates. Best-effort publish (direct xAdd); failures
  -- here never affect ride completion.
  -- Referral FCM and driver-to-driver referral reward moved to kafka-consumers
  -- RIDE_EVENTS_CONSUMER (handleReferral). See SharedLogic.RideEvents.Handlers.
  RideEventsPublisher.publishRideEnded booking ride mbRiderDetailsId validRide
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnRideCompletion {merchantId = booking.providerId, driverId = cast driverId, ride = ride, fareParameter = Just newFareParams, ..}

processEndRideFinance ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    Esq.EsqDBReplicaFlow m r,
    HasField "maxShards" r Int,
    EventStreamFlow m r,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text],
    HasField "activeDriversListKeyShards" r Int,
    Redis.HedisLTSFlowEnv r,
    BeamFlow m r
  ) =>
  Merchant ->
  Ride.Ride ->
  SRB.Booking ->
  DFare.FareParameters ->
  Id DP.Driver ->
  DI.DriverInformation ->
  TransporterConfig ->
  m ()
processEndRideFinance merchant ride booking newFareParams driverId driverInfo thresholdConfig = do
  -- Compute fare components
  let totalFare = fromMaybe 0 ride.fare
      gstAmount = fromMaybe 0 newFareParams.govtCharges
      tollAmount = fromMaybe 0 newFareParams.tollCharges
      parkingAmount = fromMaybe 0 newFareParams.parkingCharge
      baseFare = totalFare - gstAmount - tollAmount - parkingAmount
      isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
      vehicleCategoryScopedPrepaidEnabled = fromMaybe False thresholdConfig.subscriptionConfig.vehicleCategoryScopedPrepaidEnabled
      -- When wallet isolation is enabled, scope all prepaid ops to the ride's vehicle category.
      mbVehicleCategory = if vehicleCategoryScopedPrepaidEnabled then Just (Variant.castServiceTierToVehicleCategory booking.vehicleServiceTier) else Nothing

  -- Determine owner type and subscription
  mbPerson <- case ride.fleetOwnerId of
    Just _ -> pure Nothing
    Nothing -> Just <$> (QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId))
  (ownerType, ownerId) <- case ride.fleetOwnerId of
    Just fleetOwnerId -> pure (DSP.FLEET_OWNER, fleetOwnerId.getId)
    Nothing -> do
      person <- fromMaybeM (PersonNotFound driverId.getId) mbPerson
      if DCommon.checkFleetOwnerRole person.role
        then pure (DSP.FLEET_OWNER, person.id.getId)
        else pure (DSP.DRIVER, person.id.getId)
  mbPrepaidPurchase <- QSPE.findLatestActiveByOwnerAndServiceName handleSubscriptionExpiry ownerId ownerType PREPAID_SUBSCRIPTION mbVehicleCategory
  let serviceName = if isJust mbPrepaidPurchase then PREPAID_SUBSCRIPTION else YATRI_SUBSCRIPTION

  -- 1. Subscription Flow — route by serviceName
  case serviceName of
    PREPAID_SUBSCRIPTION -> processEndRidePrepaidSubscription baseFare mbVehicleCategory
    _ | thresholdConfig.subscription -> createDriverFee booking.providerId booking.merchantOperatingCityId driverId ride.fare ride.currency newFareParams driverInfo booking serviceName
    _ -> pure ()

  -- 2. Wallet Flow
  when (isPrepaidSubscriptionAndWalletEnabled || thresholdConfig.driverWalletConfig.enableDriverWallet) $ do
    createDriverWalletTransaction ride booking newFareParams driverInfo thresholdConfig mbPerson

  -- 3. Airport entry fee deduction (two ledger entries: GST then airport portion)
  when (fromMaybe False thresholdConfig.airportEntryFeeEnabled) $
    AirportEntryFee.deductAirportEntryFeeAtEndRide ride booking
  where
    processEndRidePrepaidSubscription fare mbVC = do
      case ride.fleetOwnerId of
        Just fleetOwnerId -> do
          Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey fleetOwnerId.getId) 10 10 $ do
            revenueAmount <- getPrepaidRevenueAmount fare mbVC
            _ <-
              debitPrepaidBalance
                counterpartyFleetOwner
                fleetOwnerId.getId
                fare
                revenueAmount
                ride.currency
                booking.providerId.getId
                booking.merchantOperatingCityId.getId
                booking.id.getId
                Nothing
                mbVC
                >>= fromEitherM (\err -> InternalError ("Failed to debit prepaid balance: " <> show err))
            (contributingPurchaseIds, anyExhausted) <- checkAndMarkExhaustedSubscriptions counterpartyFleetOwner fleetOwnerId.getId DSP.FLEET_OWNER mbVC
            unless (null contributingPurchaseIds) $
              QRide.updateSubscriptionPurchaseIds (Just contributingPurchaseIds) ride.id
            when anyExhausted $ do
              mbActivated <- activateNextQueuedPurchaseExpiry fleetOwnerId.getId DSP.FLEET_OWNER mbVC
              whenJust mbActivated $ \(nextPurchaseId, expiry) -> do
                now <- getCurrentTime
                let delay = diffUTCTime expiry now
                createJobIn @_ @'ExpireSubscriptionPurchase
                  (Just booking.providerId)
                  (Just booking.merchantOperatingCityId)
                  delay
                  $ ExpireSubscriptionPurchaseJobData
                    { subscriptionPurchaseId = nextPurchaseId
                    }
            pure ()
        Nothing -> do
          Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey ride.driverId.getId) 10 10 $ do
            revenueAmount <- getPrepaidRevenueAmount fare mbVC
            newBalance <-
              debitPrepaidBalance
                counterpartyDriver
                ride.driverId.getId
                fare
                revenueAmount
                ride.currency
                booking.providerId.getId
                booking.merchantOperatingCityId.getId
                booking.id.getId
                Nothing
                mbVC
                >>= fromEitherM (\err -> InternalError ("Failed to debit prepaid balance: " <> show err))
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            let balanceUpdateMessage = "Thank you for taking the ride. Your updated subscription balance is Rs." <> show newBalance
                balanceUpdatedTitle = "Subscription balance updated!"
            sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.PREPAID_BALANCE_UPDATE balanceUpdatedTitle balanceUpdateMessage driver driver.deviceToken
            (contributingPurchaseIds, anyExhausted) <- checkAndMarkExhaustedSubscriptions counterpartyDriver ride.driverId.getId DSP.DRIVER mbVC
            unless (null contributingPurchaseIds) $
              QRide.updateSubscriptionPurchaseIds (Just contributingPurchaseIds) ride.id
            when anyExhausted $ do
              mbActivated <- activateNextQueuedPurchaseExpiry ride.driverId.getId DSP.DRIVER mbVC
              whenJust mbActivated $ \(nextPurchaseId, expiry) -> do
                now' <- getCurrentTime
                let delay = diffUTCTime expiry now'
                createJobIn @_ @'ExpireSubscriptionPurchase
                  (Just booking.providerId)
                  (Just booking.merchantOperatingCityId)
                  delay
                  $ ExpireSubscriptionPurchaseJobData
                    { subscriptionPurchaseId = nextPurchaseId
                    }
            let subscriptionConfig = thresholdConfig.subscriptionConfig
            let prepaidSubscriptionThreshold = subscriptionConfig.prepaidSubscriptionThreshold
            when (newBalance < fromMaybe 0 prepaidSubscriptionThreshold) $ do
              logInfo $ "Prepaid subscription balance is less than threshold for driver: " <> show driverId.getId
              let unsubscribedMessage = "Your subscription balance is low. Please recharge to get rides"
                  unsubscribedTitle = "Low Balance Alert!"
              sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_UNSUBSCRIBED unsubscribedTitle unsubscribedMessage driver driver.deviceToken

    getPrepaidRevenueAmount fare mbVC = do
      (ownerType', ownerId') <- case ride.fleetOwnerId of
        Just fleetOwnerId -> pure (DSP.FLEET_OWNER, fleetOwnerId.getId)
        Nothing -> do
          person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          if DCommon.checkFleetOwnerRole person.role
            then pure (DSP.FLEET_OWNER, person.id.getId)
            else pure (DSP.DRIVER, person.id.getId)
      mbPurchase <- QSPE.findLatestActiveByOwnerAndServiceName handleSubscriptionExpiry ownerId' ownerType' PREPAID_SUBSCRIPTION mbVC
      let mbSyntheticPlan = Plan.mkSyntheticDriverPlanFromPurchase <$> mbPurchase
      plan <- getPlan mbSyntheticPlan PREPAID_SUBSCRIPTION booking.merchantOperatingCityId Nothing Nothing
      case plan of
        Nothing -> pure 0
        Just plan_ -> do
          let (fee, _cgst, _sgst) = calculatePlatformFeeAttr plan_.registrationAmount plan_
              totalCredit =
                case plan_.planBaseAmount of
                  PERRIDE_BASE amount -> amount
                  DAILY_BASE amount -> amount
                  WEEKLY_BASE amount -> amount
                  MONTHLY_BASE amount -> amount
                  RECHARGE_BASE amount -> amount
          if totalCredit > 0
            then pure $ (fare * fee) / totalCredit
            else pure 0

createDriverWalletTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Ride.Ride ->
  SRB.Booking ->
  DFare.FareParameters ->
  DI.DriverInformation ->
  TransporterConfig ->
  Maybe DP.Person ->
  m ()
createDriverWalletTransaction ride booking fareParams driverInfo transporterConfig mbDriver = do
  let isVat = fromMaybe False fareParams.isVatTaxType
      totalFare = fromMaybe 0 ride.fare
      rawTaxAmount = fromMaybe 0 fareParams.govtCharges -- GST or VAT (merged by FareCalculatorV2), pre-discount
      tollAmount = fromMaybe 0 fareParams.tollCharges
      tollVatAmount = fromMaybe 0 fareParams.tollFareTax -- discount does NOT apply to toll, so not scaled
      parkingAmount = fromMaybe 0 fareParams.parkingCharge
      parkingVatAmount = fromMaybe 0 fareParams.parkingChargeTax
      commissionAmount = fromMaybe 0 (ride.commission <|> booking.commission)
      mbProjectedBreakup = FC.projectFareParamsBreakup fareParams
      rawBaseFare = case mbProjectedBreakup of
        Just b -> b.discountApplicableRideFareTaxExclusive + b.nonDiscountApplicableRideFareTaxExclusive
        Nothing -> totalFare - rawTaxAmount - tollAmount - tollVatAmount - parkingAmount - parkingVatAmount
      customerDiscountAmount = fromMaybe 0 ride.discountAmount
      tipAmount = fromMaybe 0 ride.tipAmount
      -- ServiceVAT (international): platform-service VAT input credit. The base
      -- is the driver's NET taxable earning routed through the platform,
      -- which differs by payment mode:
      --   * online: full pre-discount fare flows BAP → BPP → driver; base is
      --     totalFare − commission.
      --   * cash + discount: only the BAP-subsidised discount amount actually
      --     flows through BPP (the rider's cash stays with the driver), so
      --     base is customerDiscountAmount − commission.
      --   * cash, no discount: nothing flows through BPP, so VATInput is gated
      --     off at post time ('isOnline || discount > 0'); base is effectively 0.
      -- taxAmount = VAT the customer actually paid (post-discount only), used
      -- for the VATOnline ledger leg. absorbedVat = VAT share inside the
      -- discount that BAP absorbs on the driver's behalf; posted under its
      -- own ref so it doesn't inflate the VATOnline cash path.
      (taxAmount, baseFare, absorbedVat) = case mbProjectedBreakup of
        Just b ->
          let r = RD.applyRideDiscount b customerDiscountAmount
              newBaseFare = r.postDiscountApplicableTaxExclusive + b.nonDiscountApplicableRideFareTaxExclusive
           in (r.postDiscountApplicableTax, newBaseFare, r.rideVatAbsorbedOnDiscount)
        Nothing ->
          -- Legacy GST-mode fallback: preserve exact numerical behavior
          -- (ratio on rawTaxAmount, no V2 split).
          let inclusive = rawBaseFare + rawTaxAmount
              ratio
                | customerDiscountAmount <= 0 || inclusive <= 0 = 1
                | otherwise = toRational (max 0 (inclusive - customerDiscountAmount)) / toRational inclusive
              postTax = fromRational (toRational rawTaxAmount * ratio)
              vatAbsorbed = rawTaxAmount - postTax
           in (postTax, max 0 (rawBaseFare - customerDiscountAmount), vatAbsorbed)

  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey ride.driverId.getId) 10 10 $ do
    isOnline <- do
      let forceOnline = fromMaybe False transporterConfig.driverWalletConfig.forceOnlineLedger
      -- Persist the computed ledger write mode on the booking for reconciliation
      QRB.updateLedgerWriteMode booking.id (Just forceOnline)
      if forceOnline
        then pure True
        else do
          mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId ->
            do
              CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
              >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
          case mbPaymentMethod of
            Nothing -> pure False -- Considering OFFLINE
            Just paymentMethod -> do
              case paymentMethod.paymentInstrument of
                Cash -> pure False
                BoothOnline -> pure False
                _ -> pure True

    let driverOrFleetPersonId = fromMaybe ride.driverId ride.fleetOwnerId

    let panLinkTdsEnabled = panAadhaarLinkTdsEnabled transporterConfig.taxConfig
        configTdsRate = (.rate) <$> transporterConfig.taxConfig.defaultTdsRate
    mbTdsRate <- case ride.fleetOwnerId of
      Just fleetOwnerId -> do
        mbFleetInfo <- QFOI.findByPrimaryKey (cast fleetOwnerId)
        let currentRate = mbFleetInfo >>= (.tdsRate)
        unless panLinkTdsEnabled $
          whenJust mbFleetInfo $ \_ ->
            when (isNothing currentRate) $
              whenJust configTdsRate $ \rate ->
                QFOI.updateTdsRate (Just rate) (cast fleetOwnerId)
        pure $ if panLinkTdsEnabled then currentRate else (currentRate <|> configTdsRate)
      Nothing -> do
        let currentRate = driverInfo.tdsRate
        when (not panLinkTdsEnabled && isNothing currentRate) $
          whenJust configTdsRate $ \rate ->
            QDI.updateTdsRate (Just rate) ride.driverId
        pure $ if panLinkTdsEnabled then currentRate else (currentRate <|> configTdsRate)

    mbPanCard <- QPanCard.findByDriverId driverOrFleetPersonId
    -- For threshold-benefit gating (section 194O), look up the counterparty's
    -- cumulative earnings. Driver rides use driver_stats.totalEarnings (lifetime
    -- ride.fare sum, interim — TODO(MSIL-TDS-FY) switch to FY-scoped).
    -- Fleet rides return Nothing → applyThresholdBenefit skips the gate (always
    -- deducts) until a fleet accumulator is added.
    mbCumulativeEarnings <- case ride.fleetOwnerId of
      Just _ -> pure Nothing
      Nothing -> do
        mbStats <- QDriverStats.findByPrimaryKey (cast ride.driverId)
        pure $ (.totalEarnings) <$> mbStats
    let effectiveTdsRate = computeEffectiveTdsRate mbPanCard mbTdsRate transporterConfig.taxConfig
        baseFareForTds = max 0 baseFare
        mbTdsAmount = do
          rate <- effectiveTdsRate
          let rawAmount = baseFareForTds * realToFrac rate -- tdsRate is already decimal (0.01 = 1%)
              gatedAmount = applyThresholdBenefit transporterConfig.taxConfig mbCumulativeEarnings mbPanCard baseFareForTds rawAmount
          if gatedAmount > 0 then Just gatedAmount else Nothing

    let serviceVatAmount =
          case transporterConfig.taxConfig.serviceVatPercentage of
            Just pct
              | isVat ->
                let baseForServiceVat =
                      if isOnline
                        then case mbProjectedBreakup of
                          Just b -> RD.projectFareParamsBreakupTotal b - commissionAmount
                          Nothing -> totalFare - commissionAmount
                        else max 0 (customerDiscountAmount - commissionAmount)
                 in HighPrecMoney (baseForServiceVat.getHighPrecMoney * (toRational pct / 100))
            _ -> HighPrecMoney 0.0

    merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist booking.merchantOperatingCityId.getId)
    ctx <- buildFinanceCtx booking ride mbDriver mbPanCard (Just driverInfo) transporterConfig isOnline
    let tollWithVat = tollAmount + tollVatAmount
    let parkingWithVat = parkingAmount + parkingVatAmount
    let mkRideLineItems clubVatInclusive issuedToType =
          let rideInclusiveLine = rawBaseFare + rawTaxAmount
              tollInclusiveLine = tollAmount + tollVatAmount
              parkingInclusiveLine = parkingAmount + parkingVatAmount
              -- Tagged Fare/Tax pair: groupId pairs the two lines so the renderer
              -- collapses them into one main-table row.
              mkPair gId ty isExt desc descType amt =
                if amt > 0
                  then
                    Just
                      InvoiceLineItem
                        { description = desc,
                          descriptionType = Just descType,
                          quantity = 1,
                          unitPrice = amt,
                          lineTotal = amt,
                          isExternalCharge = isExt,
                          groupId = Just gId,
                          itemType = Just ty
                        }
                  else Nothing
              -- Standalone Fare line, no Tax pair (groupId = Nothing).
              mkStandaloneFare desc descType amt =
                if amt > 0
                  then
                    Just
                      InvoiceLineItem
                        { description = desc,
                          descriptionType = Just descType,
                          quantity = 1,
                          unitPrice = amt,
                          lineTotal = amt,
                          isExternalCharge = False,
                          groupId = Nothing,
                          itemType = Just Fare
                        }
                  else Nothing
              -- Adjustment line (Tip positive / Platform Commission negative).
              -- Renders below "Total" in the right-block ladder; contributes to Net Total.
              mkAdjustment desc descType signedAmt =
                if signedAmt /= 0
                  then
                    Just
                      InvoiceLineItem
                        { description = desc,
                          descriptionType = Just descType,
                          quantity = 1,
                          unitPrice = signedAmt,
                          lineTotal = signedAmt,
                          isExternalCharge = False,
                          groupId = Nothing,
                          itemType = Just Adjustment
                        }
                  else Nothing
              rideAndTollLines =
                if clubVatInclusive
                  then
                    [ mkPair "g-ride" Fare False "Ride Fare (Incl. VAT)" RideFareInclVat rideInclusiveLine,
                      mkPair "g-toll" Fare True "Toll Fare (Incl. VAT)" TollFareInclVat tollInclusiveLine,
                      mkPair "g-parking" Fare True "Parking Charges (Incl. VAT)" ParkingChargesInclVat parkingInclusiveLine
                    ]
                  else
                    [ mkPair "g-ride" Fare False "Base Fare" BaseFare rawBaseFare,
                      mkPair "g-ride" Tax False "Tax" RideTax rawTaxAmount,
                      mkPair "g-toll" Fare True "Toll Charges" TollCharges tollAmount,
                      mkPair "g-toll" Tax True "Toll Charges Tax" TollChargesTax tollVatAmount,
                      mkPair "g-parking" Fare True "Parking Charges" ParkingCharges parkingAmount,
                      mkPair "g-parking" Tax True "Parking Charges Tax" ParkingChargesTax parkingVatAmount
                    ]
              showVatInput = isVat && maybe False (fromMaybe False . (.showVatInputLineItem)) transporterConfig.invoiceConfig
              commonLines =
                [ mkAdjustment "Tip" Tip tipAmount,
                  if issuedToType == CUSTOMER then Nothing else mkAdjustment "Platform Commission" PlatformCommission (negate commissionAmount),
                  if showVatInput then mkStandaloneFare "VAT Input" VatInput serviceVatAmount else Nothing
                ]
           in catMaybes (rideAndTollLines <> commonLines)
        rideGstBreakdown =
          computeGstBreakdownByPlace
            transporterConfig.taxConfig.rideGst
            (Just $ show merchantOperatingCity.state)
            booking.fromLocation.address.state
            (Just $ show merchantOperatingCity.city)
            booking.fromLocation.address.city
            (taxAmount + absorbedVat)
        -- CUSTOMER invoice: never club VAT into the ride/toll/parking lines —
        -- riders get the itemised view regardless of transporter config.
        customerInvoiceConfig =
          InvoiceConfig
            { invoiceType = BeckInvoice.Ride,
              issuedToType = CUSTOMER,
              issuedToId = maybe "" (.getId) booking.riderId,
              issuedToName = booking.riderName,
              issuedToAddress = booking.fromLocation.address.fullAddress,
              lineItems = mkRideLineItems False CUSTOMER,
              gstBreakdown = rideGstBreakdown,
              referenceId = Nothing,
              isVat = isVat,
              issuedToTaxNo = Nothing,
              issuedByTaxNo = Nothing,
              paymentMode = Just (if isOnline then "ONLINE" else "CASH"),
              periodStart = Nothing,
              periodEnd = Nothing
            }
        -- DRIVER / FLEET_OWNER copy of the same Ride invoice. Honors
        -- driverInvoiceLineItemsVatInclusive so the supplier-side invoice
        -- can club VAT into a single inclusive line per the transporter
        -- config. Created standalone (no extra ledger entries) so it doesn't
        -- duplicate the indirect-tax transactions emitted for the customer
        -- invoice's linked entries.
        driverClubVatInclusive = maybe False (.driverInvoiceLineItemsVatInclusive) transporterConfig.invoiceConfig
        driverInvoiceConfig =
          InvoiceConfig
            { invoiceType = BeckInvoice.Ride,
              issuedToType = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER,
              issuedToId = driverOrFleetPersonId.getId,
              issuedToName = mbDriver <&> (.firstName),
              issuedToAddress = Nothing,
              lineItems = mkRideLineItems driverClubVatInclusive (if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER),
              gstBreakdown = rideGstBreakdown,
              referenceId = Nothing,
              isVat = isVat,
              issuedToTaxNo = Nothing,
              issuedByTaxNo = Nothing,
              paymentMode = Just (if isOnline then "ONLINE" else "CASH"),
              periodStart = Nothing,
              periodEnd = Nothing
            }
    let taxRefOnline = if isVat then walletReferenceVATOnline else walletReferenceGSTOnline
        taxRefCash = if isVat then walletReferenceVATCash else walletReferenceGSTCash
        tdsRef = if isOnline then walletReferenceTDSDeductionOnline else walletReferenceTDSDeductionCash
    -- Uniform accounting model — same ledger entries are posted regardless of
    -- payment mode; for cash rides, the ride-earning + Tips entries are
    -- immediately reversed so the net balance lands at 0 (driver got the
    -- money from the rider directly; BPP's books didn't hold it). See the
    -- module-level table at the top of this file for the full matrix.
    result <- runFinance ctx $ do
      -- Ride-earning components. Online: 2-leg pass-through (BAP cash in via
      -- BuyerExternal, then credited to driver wallet). Cash: single leg
      -- BuyerControl → OwnerControl — dedicated tracking accounts so A/R and
      -- driver wallet stay untouched for cash that flowed rider → driver
      -- directly.
      let postEarning (amt, ref) =
            if isOnline
              then do
                transfer_ BuyerAsset BuyerExternal amt ref
                void $ transfer BuyerExternal OwnerLiability amt ref
              else void $ transfer BuyerControl OwnerControl amt ref
      if isVat
        then do
          let rideEarningComponents =
                [ (baseFare, walletReferenceBaseRide),
                  (taxAmount, if isOnline then taxRefOnline else taxRefCash),
                  (tollWithVat, walletReferenceTollCharges),
                  (parkingWithVat, walletReferenceParkingCharges)
                ]
          forM_ rideEarningComponents postEarning
        else do
          let rideEarningComponents =
                [ (baseFare, walletReferenceBaseRide),
                  (tollWithVat, walletReferenceTollCharges),
                  (parkingWithVat, walletReferenceParkingCharges)
                ]
          forM_ rideEarningComponents postEarning
          -- GST tax: online → BAP routes customer's GST to govt via BPP (2-leg);
          --          cash   → driver remits cash-collected GST from wallet.
          if isOnline
            then do
              transfer_ BuyerAsset BuyerExternal taxAmount taxRefOnline
              void $ transfer BuyerExternal GovtIndirect taxAmount taxRefOnline
            else void $ transfer OwnerLiability GovtIndirect taxAmount taxRefCash
      -- TDS — driver wallet reduces in both modes (cash driver owes platform).
      whenJust mbTdsAmount $ \tdsAmount ->
        void $ transfer OwnerLiability GovtDirect tdsAmount tdsRef
      when (isVat && serviceVatAmount > 0) $
        void $ transferWithoutAttribution GovtExpense OwnerLiability serviceVatAmount walletReferenceVATInput
      -- BAP subsidy — BAP remits to BPP in both modes (2-leg pass-through); credits driver wallet.
      let discountsRef = if isOnline then walletReferenceDiscountsOnline else walletReferenceDiscountsCash
      when (customerDiscountAmount > 0) $ do
        transfer_ BuyerAsset BuyerExternal customerDiscountAmount discountsRef
        void $ transfer BuyerExternal OwnerLiability customerDiscountAmount discountsRef
      -- Tip — online: BuyerAsset → OwnerLiability (1 leg). Cash: Control.
      when (tipAmount > 0) $
        if isOnline
          then void $ transfer BuyerAsset OwnerLiability tipAmount walletReferenceTips
          else void $ transfer BuyerControl OwnerControl tipAmount walletReferenceTips
      invoice customerInvoiceConfig
    case result of
      Left err -> fromEitherM (\e -> InternalError ("Failed to create wallet transaction: " <> show e)) (Left err)
      Right (mbInvoiceId, _entryIds) -> do
        let mbInvoiceIdText = (.getId) <$> mbInvoiceId
        QRB.updateFinanceInvoiceId booking.id mbInvoiceIdText

    -- Standalone driver / fleet-owner Ride invoice mirroring the customer
    -- invoice. No transfers in this block → no ledger entries are linked,
    -- so no duplicate IndirectTaxTransaction is emitted.
    driverInvoiceResult <- runFinance ctx $ invoice driverInvoiceConfig
    case driverInvoiceResult of
      Left err -> fromEitherM (\e -> InternalError ("Failed to create driver ride invoice: " <> show e)) (Left err)
      Right _ -> pure ()

    let commissionAlreadyCollectedAtBooth = booking.fareSettlementType == Just CommissionOnly
    when (commissionAmount > 0) $ do
      let commissionRef = if isOnline then walletReferenceCommissionOnline else walletReferenceCommissionCash
          onlineCommissionPaidOutDirectly = isOnline && fromMaybe False transporterConfig.driverWalletConfig.onlineCommissionPaidOutDirectly
          shouldReverseCommissionWalletDebit = onlineCommissionPaidOutDirectly || commissionAlreadyCollectedAtBooth
          commissionInvoiceConfig =
            InvoiceConfig
              { invoiceType = BeckInvoice.Commission,
                issuedToType = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER,
                issuedToId = driverOrFleetPersonId.getId,
                issuedToName = mbDriver <&> (.firstName),
                issuedToAddress = Nothing,
                referenceId = Nothing,
                lineItems =
                  catMaybes
                    [ Just InvoiceLineItem {description = "Platform Commission", descriptionType = Just PlatformCommission, quantity = 1, unitPrice = commissionAmount, lineTotal = commissionAmount, isExternalCharge = False, groupId = Just "g-commission", itemType = Just Fare}
                    ],
                gstBreakdown = Nothing,
                isVat = isVat,
                issuedToTaxNo = Nothing,
                issuedByTaxNo = Nothing,
                paymentMode = Nothing, -- Commission is deducted from driver earnings; no rider payment involved.
                periodStart = Nothing,
                periodEnd = Nothing
              }
      commissionResult <- runFinance ctx $ do
        void $ transfer OwnerLiability SellerRevenue commissionAmount commissionRef
        when shouldReverseCommissionWalletDebit $
          void $ transfer SellerRevenue OwnerLiability commissionAmount walletReferenceDeductedAtPaymentByPlatform
        invoice commissionInvoiceConfig
      case commissionResult of
        Left err -> fromEitherM (\e -> InternalError ("Failed to create commission invoice: " <> show e)) (Left err)
        Right _ -> pure ()

makeWalletRunningBalanceLockKey :: Text -> Text
makeWalletRunningBalanceLockKey personId = "WalletRunningBalanceLockKey:" <> personId

makeDriverLeaderBoardKey :: LConfig.LeaderBoardType -> Bool -> Id DMOC.MerchantOperatingCity -> Day -> Day -> Text
makeDriverLeaderBoardKey leaderBoardType isCached merchantOpCityId fromDate toDate =
  case leaderBoardType of
    LConfig.DAILY -> if isCached then makeCachedDailyDriverLeaderBoardKey else makeDailyDriverLeaderBoardKey
    LConfig.WEEKLY -> if isCached then makeCachedWeeklyDriverLeaderBoardKey else makeWeeklyDriverLeaderBoardKey
    LConfig.MONTHLY -> if isCached then makeCachedMonthlyDriverLeaderBoardKey else makeMonthlyDriverLeaderBoardKey
  where
    makeCachedDailyDriverLeaderBoardKey :: Text
    makeCachedDailyDriverLeaderBoardKey = "DDLBCK:" <> merchantOpCityId.getId <> ":" <> show fromDate

    makeDailyDriverLeaderBoardKey :: Text
    makeDailyDriverLeaderBoardKey = "DDLBK:" <> merchantOpCityId.getId <> ":" <> show fromDate

    makeCachedWeeklyDriverLeaderBoardKey :: Text
    makeCachedWeeklyDriverLeaderBoardKey = "DWLBCK:" <> merchantOpCityId.getId <> ":" <> show fromDate <> ":" <> show toDate

    makeWeeklyDriverLeaderBoardKey :: Text
    makeWeeklyDriverLeaderBoardKey = "DWLBK:" <> merchantOpCityId.getId <> ":" <> show fromDate <> ":" <> show toDate

    makeMonthlyDriverLeaderBoardKey :: Text
    makeMonthlyDriverLeaderBoardKey =
      let month = getMonth fromDate
       in "DMLBK:" <> merchantOpCityId.getId <> ":" <> show month

    makeCachedMonthlyDriverLeaderBoardKey :: Text
    makeCachedMonthlyDriverLeaderBoardKey =
      let month = getMonth fromDate
       in "DMLBCK:" <> merchantOpCityId.getId <> ":" <> show month

getRidesAndDistancefromZscore :: Double -> Int -> (Int, Meters)
getRidesAndDistancefromZscore dzscore dailyZscoreBase =
  let (totalRides, totalDistance) = quotRem (double2Int dzscore) dailyZscoreBase
   in (totalRides, Meters totalDistance)

getCurrentDate :: UTCTime -> Day
getCurrentDate time =
  let currentDate = localDay $ utcToLocalTime timeZoneIST time
   in currentDate

getMonth :: Day -> Int
getMonth = (\(_, m, _) -> m) . toGregorian

putDiffMetric :: (Metrics.HasBPPMetrics m r, CacheFlow m r, EsqDBFlow m r) => Id Merchant -> HighPrecMoney -> Meters -> m ()
putDiffMetric merchantId money mtrs = do
  org <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  Metrics.putFareAndDistanceDeviations org.name (roundToIntegral money) mtrs

getRouteAndDistanceBetweenPoints ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasKafkaProducer r
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LatLong ->
  LatLong ->
  [LatLong] ->
  Meters ->
  m ([LatLong], Meters)
getRouteAndDistanceBetweenPoints merchantId merchantOpCityId origin destination interpolatedPoints estimatedDistance = do
  -- somehow interpolated points pushed to redis in reversed order, so we need to reverse it back
  let pickedWaypoints = origin :| (pickWaypoints interpolatedPoints <> [destination])
  logTagInfo "endRide" $ "pickedWaypoints: " <> show pickedWaypoints
  routeResponse <-
    Maps.getRoutes merchantId merchantOpCityId Nothing $
      Maps.GetRoutesReq
        { waypoints = pickedWaypoints,
          mode = Just Maps.CAR,
          calcPoints = True
        }
  let mbShortestRoute = getRouteInfoWithShortestDuration routeResponse
      routePoints = maybe [] (.points) mbShortestRoute
      distance = maybe estimatedDistance (\route -> fromMaybe estimatedDistance route.distance) mbShortestRoute
  -- Next error is impossible, because we never receive empty list from directions api
  --mbShortestRouteDistance & fromMaybeM (InvalidRequest "Couldn't calculate route distance")
  return (routePoints, distance)

-- TODO reuse code from rider-app
getRouteInfoWithShortestDuration :: [Maps.RouteInfo] -> Maybe Maps.RouteInfo
getRouteInfoWithShortestDuration [] = Nothing
getRouteInfoWithShortestDuration (routeInfo : routeInfoArray) =
  if null routeInfoArray
    then Just routeInfo
    else do
      restRouteResult <- getRouteInfoWithShortestDuration routeInfoArray
      Just $ comparator routeInfo restRouteResult
  where
    comparator route1 route2 =
      if route1.duration < route2.duration
        then route1
        else route2

-- for distance api we can't pick more than 10 waypoints
pickWaypoints :: [a] -> [a]
pickWaypoints waypoints = do
  let step = length waypoints `div` 7
  take 7 $ foldr (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list) [] $ zip [1 ..] waypoints

-- here pickNWayPoints is used to pick n waypoints from the list of waypoints
pickNWayPoints :: Int -> [a] -> [a]
pickNWayPoints number waypoints
  | null waypoints = []
  | number <= 1 = [last waypoints]
  | length waypoints == 1 = waypoints
  | otherwise = do
    let len = length waypoints
        step = len `div` number
        pickedPoints =
          take (number - 1) $
            foldr
              (\(n, waypoint) list -> if n `safeMod` step == 0 then waypoint : list else list)
              []
              (zip [1 ..] waypoints)
    pickedPoints ++ [last waypoints]

pickedWaypointsForEditDestination :: [(LatLong, Bool)] -> Int -> [LatLong]
pickedWaypointsForEditDestination waypoints numPointsToAdd =
  let n = length waypoints -- Total number of waypoints
      chunks = breakOnTrueInclude waypoints
      remainingPicks = numPointsToAdd :: Int
      weightedChunks =
        [pickNWayPoints (max 1 (remainingPicks * length chunk `div` n)) chunk | chunk <- chunks]
   in concat weightedChunks

breakOnTrueInclude :: [(LatLong, Bool)] -> [[LatLong]]
breakOnTrueInclude [] = []
breakOnTrueInclude latlongs =
  -- take elements until we find a true an
  foldr
    ( \(latlong, bool') acc ->
        if bool'
          then [latlong] : acc
          else case acc of
            [] -> [[latlong]]
            (x : xs) -> (latlong : x) : xs
    )
    []
    latlongs

safeMod :: Int -> Int -> Int
_ `safeMod` 0 = 0
a `safeMod` b = a `mod` b

createDriverFee ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasField "schedulerType" r SchedulerType,
    HasField "activeDriversListKeyShards" r Int,
    HasKafkaProducer r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DP.Driver ->
  Maybe HighPrecMoney ->
  Currency ->
  DFare.FareParameters ->
  DI.DriverInformation ->
  SRB.Booking ->
  ServiceNames ->
  m ()
createDriverFee merchantId merchantOpCityId driverId rideFare currency newFareParams driverInfo booking serviceName = do
  unless (newFareParams.platformFeeChargesBy == DFP.None) $ do
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    fleetDriverAssoc <- QFDAE.findByDriverId driverId True
    fleetOwnerInfo <- maybe (pure Nothing) (\fda -> QFOI.findByPrimaryKey (Id fda.fleetOwnerId)) fleetDriverAssoc
    let fleetIsSubscriptionEligble = maybe True (.isEligibleForSubscription) fleetOwnerInfo
    freeTrialDaysLeft' <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
    let govtCharges = fromMaybe 0.0 newFareParams.govtCharges
    let chargeBy = if not fleetIsSubscriptionEligble then DFP.NoCharge else newFareParams.platformFeeChargesBy
    case chargeBy of
      DFP.NoCharge -> pure ()
      _ -> createDriverFee' transporterConfig freeTrialDaysLeft' govtCharges
  where
    createDriverFee' transporterConfig freeTrialDaysLeft' govtCharges = do
      (platformFee, cgst, sgst, isSpecialZoneCharge) <- case newFareParams.platformFeeChargesBy of
        DFP.SlabBased -> case newFareParams.fareParametersDetails of
          DFare.SlabDetails fpDetails -> return (fromMaybe 0 fpDetails.platformFee, fromMaybe 0 fpDetails.cgst, fromMaybe 0 fpDetails.sgst, True)
          _ -> return (0, 0, 0, False)
        DFP.Subscription -> return (0, 0, 0, False)
        DFP.FixedAmount -> return (fromMaybe 0.0 newFareParams.platformFee, fromMaybe 0.0 newFareParams.cgst, fromMaybe 0.0 newFareParams.sgst, True)
        _ -> return (0, 0, 0, False)
      let totalDriverFee = govtCharges + platformFee + cgst + sgst
      now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      let currentVehicleCategory = Just $ Variant.castVehicleVariantToVehicleCategory $ Variant.castServiceTierToVariant booking.vehicleServiceTier
      subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId (Just booking.configInExperimentVersions) serviceName
      let isPlanMandatoryForVariant = maybe False (\vcList -> isJust $ DL.find (\enabledVc -> maybe False (enabledVc ==) currentVehicleCategory) vcList) (subscriptionConfig >>= (.executionEnabledForVehicleCategories))
      (mbDriverPlan, isOnFreeTrial) <- getPlanAndPushToDefualtIfEligible transporterConfig subscriptionConfig freeTrialDaysLeft' isSpecialZoneCharge isPlanMandatoryForVariant currentVehicleCategory
      let enableCityBasedFeeSwitch = fromMaybe False $ subscriptionConfig <&> (.enableCityBasedFeeSwitch)
      driverFee <- mkDriverFee serviceName now Nothing Nothing merchantId driverId rideFare govtCharges platformFee cgst sgst currency transporterConfig (Just booking) isSpecialZoneCharge currentVehicleCategory subscriptionConfig
      lastElderSiblingDriverFee <- QDF.findLatestFeeByDriverIdAndServiceName driverId serviceName merchantOpCityId driverFee.startTime driverFee.endTime enableCityBasedFeeSwitch
      restSiblingDriverFee <- do
        case lastElderSiblingDriverFee of
          Just lESDriverFee -> do
            if lESDriverFee.hasSibling == Just True
              then QDF.findAllChildsOFDriverFee merchantOpCityId driverFee.startTime driverFee.endTime DF.ONGOING serviceName [lESDriverFee.id] enableCityBasedFeeSwitch
              else return []
          Nothing -> return []
      let lastDriverFee = DL.find (\dfee -> (Just dfee.vehicleCategory) == currentVehicleCategory) (restSiblingDriverFee <> catMaybes [lastElderSiblingDriverFee])
      let isEnableForVariant = maybe False (\vcList -> isJust $ DL.find (\enabledVc -> maybe False (enabledVc ==) currentVehicleCategory) vcList) (subscriptionConfig >>= (.executionEnabledForVehicleCategories))
      let toUpdateOrCreateDriverfee = (totalDriverFee > 0 || (totalDriverFee <= 0 && isPlanMandatoryForVariant && isJust mbDriverPlan)) && isEnableForVariant
      when (toUpdateOrCreateDriverfee && isEligibleForCharge transporterConfig isOnFreeTrial isSpecialZoneCharge) $ do
        logDebug $ "createDriverFee': updating or creating driverfee: " <> show driverFee
        numRides <- case lastDriverFee of
          Just ldFee ->
            if now >= ldFee.startTime && now < ldFee.endTime
              then do
                QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst True booking isSpecialZoneCharge
                return (ldFee.numRides + 1)
              else do
                createWithMbSibling driverFee lastElderSiblingDriverFee ldFee
                return 1
          Nothing -> do
            createWithMbSibling driverFee lastElderSiblingDriverFee driverFee
            return 1
        when (fromMaybe False (subscriptionConfig >>= (.isVendorSplitEnabled))) $ do
          let vehicleVariant = Variant.castServiceTierToVariant booking.vehicleServiceTier
          allVendorSplitDetails <- CQVSD.findAllByAreaIncludingDefaultAndCityAndVariant booking.area merchantOpCityId vehicleVariant
          let allVendorSplitDetailsExcludingDailyPlan = DL.filter (\d -> isNothing d.dailyPlanId) allVendorSplitDetails
          let vendorSplitDetails = case booking.area of
                Just area ->
                  let areaDetails = DL.filter (\detail -> detail.area == area) allVendorSplitDetailsExcludingDailyPlan
                      baseAreaDetails =
                        if null areaDetails && hasGateId area
                          then DL.filter (\detail -> detail.area == stripGateId area) allVendorSplitDetailsExcludingDailyPlan
                          else areaDetails
                   in if null baseAreaDetails
                        then DL.filter (\detail -> detail.area == Default) allVendorSplitDetailsExcludingDailyPlan
                        else baseAreaDetails
                Nothing -> DL.filter (\detail -> detail.area == Default) allVendorSplitDetailsExcludingDailyPlan
          unless (null vendorSplitDetails) $ do
            let vendorFeeBase = platformFee + cgst + sgst
                vendorData = DL.map (\vendor -> (vendor.vendorId, vendor.splitType, toRational vendor.splitValue, vendor.maxVendorFeeAmount)) vendorSplitDetails
                -- Pass vendor fee along with its maxVendorFeeAmount limit for cumulative validation
                vendorFeesWithLimit = DL.map (\(vendorId, splitType, amount, maxLimit) -> (mkVendorFee (maybe driverFee.id (.id) lastDriverFee) now vendorFeeBase (vendorId, splitType, amount, maxLimit), maxLimit)) vendorData
            unless (null vendorFeesWithLimit) $ do
              case lastDriverFee of
                Just ldFee | now >= ldFee.startTime && now < ldFee.endTime -> QVF.updateManyVendorFeeWithMaxLimit merchantOpCityId vendorFeesWithLimit
                _ -> QVF.createManyWithMaxLimit vendorFeesWithLimit

        plan <- getPlan mbDriverPlan serviceName merchantOpCityId Nothing currentVehicleCategory
        fork "Sending switch plan nudge" $ PaymentNudge.sendSwitchPlanNudge transporterConfig driverInfo plan mbDriverPlan numRides serviceName
        scheduleJobs transporterConfig driverFee merchantId merchantOpCityId now
    mkVendorFee driverFeeId now vendorFeeBase (vendorId, splitType, splitValue, _) =
      let amount = case splitType of
            DVSD.FIXED -> HighPrecMoney splitValue
            DVSD.PERCENTAGE -> vendorFeeBase * HighPrecMoney (splitValue / 100)
       in DVF.VendorFee {amount = amount, driverFeeId = driverFeeId, vendorId = vendorId, createdAt = now, updatedAt = now}
    isEligibleForCharge transporterConfig isOnFreeTrial isSpecialZoneCharge = do
      let notOnFreeTrial = not isOnFreeTrial
      if isSpecialZoneCharge
        then transporterConfig.considerSpecialZoneRideChargesInFreeTrial || notOnFreeTrial
        else notOnFreeTrial

    getPlanAndPushToDefualtIfEligible :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> Maybe DSC.SubscriptionConfig -> Int -> Bool -> Bool -> Maybe DVC.VehicleCategory -> m (Maybe DriverPlan, Bool)
    getPlanAndPushToDefualtIfEligible transporterConfig mbSubsConfig freeTrialDaysLeft' isSpecialZoneCharge planMandatory currentVehicleCategory = do
      mbDriverPlan' <- findByDriverIdWithServiceName (cast driverId) serviceName
      (isOnFreeTrial', _) <- do
        case mbSubsConfig of
          Just subsConfig -> Plan.isOnFreeTrial driverId subsConfig freeTrialDaysLeft' mbDriverPlan'
          Nothing -> return (True, Nothing)
      let chargeSPZRides = transporterConfig.considerSpecialZoneRideChargesInFreeTrial
          isEligibleForDefaultPlanAfterFreeTrial = not isOnFreeTrial' && planMandatory && transporterConfig.allowDefaultPlanAllocation
          isEligibleForDefaultPlanBeforeFreeTrial = isOnFreeTrial' && chargeSPZRides && planMandatory
      if isNothing mbDriverPlan'
        then do
          case (isSpecialZoneCharge, isEligibleForDefaultPlanBeforeFreeTrial, isEligibleForDefaultPlanAfterFreeTrial) of
            (True, True, _) -> (,isOnFreeTrial') <$> assignDefaultPlan currentVehicleCategory
            (_, _, True) -> (,isOnFreeTrial') <$> assignDefaultPlan currentVehicleCategory
            _ -> return (mbDriverPlan', isOnFreeTrial')
        else return (mbDriverPlan', isOnFreeTrial')
    assignDefaultPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe DVC.VehicleCategory -> m (Maybe DriverPlan)
    assignDefaultPlan currentVehicleCategory = do
      case currentVehicleCategory of
        Just vc -> do
          plans <- CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName vc False
          case plans of
            (plan' : _) -> do
              newDriverPlan <- Plan.mkDriverPlan plan' (driverId, merchantId, merchantOpCityId)
              QDPlan.create newDriverPlan
              Plan.updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (Just DI.PENDING) Nothing
              QDI.updatPayerVpa Nothing (cast driverId)
              return $ Just newDriverPlan
            _ -> return Nothing
        Nothing -> return Nothing

    createWithMbSibling :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DF.DriverFee -> Maybe DF.DriverFee -> DF.DriverFee -> m ()
    createWithMbSibling driverFee lastElderSiblingDriverFee ldFee = do
      let elderSiblingId = if (lastElderSiblingDriverFee <&> (.id)) == Just ldFee.id then Nothing else lastElderSiblingDriverFee <&> (.id)
      whenJust elderSiblingId $ \elderSiblingId' -> QDF.updateHasSiblingInDriverFee elderSiblingId'
      let driverFeeToCreate = driverFee{siblingFeeId = elderSiblingId}
      QDF.create driverFeeToCreate

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType, HasField "activeDriversListKeyShards" r Int) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Id MerchantOperatingCity -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId merchantOpCityId now = do
  logDebug $ "scheduleJobs: for driverFee" <> show driverFee
  void $
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> pure ()
      Just dfCalcTime -> Redis.runInMasterCloudRedisCell $ do
        whenWithLockRedis (mkLockKeyForDriverFeeCalculation driverFee.startTime driverFee.endTime merchantOpCityId) 60 $ do
          isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName
          let dfCalculationJobTs = diffUTCTime (addUTCTime dfCalcTime driverFee.endTime) now
          case isDfCaclculationJobScheduled of
            ----- marker ---
            Nothing -> do
              logDebug $ "scheduleJobs: creating job for driverFee" <> show driverFee
              shardNums <- ADL.getShardNumsForFanOut
              forM_ shardNums $ \shardNum ->
                createJobIn @_ @'CalculateDriverFees (Just merchantId) (Just merchantOpCityId) dfCalculationJobTs $
                  CalculateDriverFeesJobData
                    { merchantId = merchantId,
                      merchantOperatingCityId = Just merchantOpCityId,
                      startTime = driverFee.startTime,
                      serviceName = Just (driverFee.serviceName),
                      scheduleNotification = Just True,
                      scheduleOverlay = Just True,
                      scheduleManualPaymentLink = Just True,
                      scheduleDriverFeeCalc = Just True,
                      createChildJobs = Just True,
                      recalculateManualReview = Nothing,
                      endTime = driverFee.endTime,
                      shardNum = shardNum
                    }
              setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName dfCalculationJobTs
            _ -> do
              logDebug $ "scheduleJobs: job already scheduled for driverFee" <> show driverFee
              pure ()

mkDriverFee ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  ServiceNames ->
  UTCTime ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Id Merchant ->
  Id DP.Driver ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  HighPrecMoney ->
  Currency ->
  TransporterConfig ->
  Maybe SRB.Booking ->
  Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe DSC.SubscriptionConfig ->
  m DF.DriverFee
mkDriverFee serviceName now startTime' endTime' merchantId driverId rideFare govtCharges platformFee cgst sgst currency transporterConfig _mbBooking isSpecialZoneCharge currentVehicleCategory subsConfig = do
  id <- generateGUID
  nowUtc <- getCurrentTime
  let potentialStart = addUTCTime transporterConfig.driverPaymentCycleStartTime (UTCTime (utctDay now) (secondsToDiffTime 0))
      startTime = if now >= potentialStart then potentialStart else addUTCTime (-1 * transporterConfig.driverPaymentCycleDuration) potentialStart
      endTime = addUTCTime transporterConfig.driverPaymentCycleDuration startTime
      payBy = if isNothing transporterConfig.driverFeeCalculationTime then addUTCTime transporterConfig.driverPaymentCycleBuffer endTime else addUTCTime (transporterConfig.driverAutoPayNotificationTime + transporterConfig.driverAutoPayExecutionTime) endTime
      platformFee_ = if isNothing transporterConfig.driverFeeCalculationTime then DF.PlatformFee {fee = platformFee, cgst, sgst, currency} else DF.PlatformFee {fee = 0, cgst = 0, sgst = 0, currency}
      govtCharges_ = if isNothing transporterConfig.driverFeeCalculationTime then govtCharges else 0
      isPlanMandatory = maybe False (\vcList -> isJust $ DL.find (\enabledVc -> maybe False (enabledVc ==) currentVehicleCategory) vcList) (subsConfig >>= (.executionEnabledForVehicleCategories))
      totalFee = platformFee + cgst + sgst
      (specialZoneRideCount, specialZoneAmount) = specialZoneMetricsIntialization totalFee
      numRides = if serviceName == YATRI_SUBSCRIPTION then 1 else 0
  mbDriverPlan <- findByDriverIdWithServiceName (cast driverId) serviceName -- what if its changed? needed inside lock?
  plan <- getPlan mbDriverPlan serviceName transporterConfig.merchantOperatingCityId Nothing currentVehicleCategory
  return $
    DF.DriverFee
      { status = DF.ONGOING,
        collectedBy = Nothing,
        collectedAt = Nothing,
        createdAt = nowUtc,
        updatedAt = nowUtc,
        platformFee = platformFee_,
        totalEarnings = fromMaybe 0 rideFare,
        feeType = case (plan, isPlanMandatory) of
          (Nothing, _) -> DF.RECURRING_INVOICE
          (Just plan_, True) -> if plan_.paymentMode == MANUAL then DF.RECURRING_INVOICE else DF.RECURRING_EXECUTION_INVOICE
          (Just _, False) -> DF.RECURRING_INVOICE,
        govtCharges = govtCharges_,
        offerId = Nothing,
        planOfferTitle = Nothing,
        autopayPaymentStage = Nothing,
        stageUpdatedAt = Nothing,
        billNumber = Nothing,
        schedulerTryCount = 0,
        feeWithoutDiscount = Nothing, -- Only for NY rn
        overlaySent = False,
        amountPaidByCoin = Nothing,
        planId = plan <&> (.id),
        planMode = plan <&> (.paymentMode),
        notificationRetryCount = 0,
        badDebtDeclarationDate = Nothing,
        badDebtRecoveryDate = Nothing,
        vehicleNumber = case mbDriverPlan <&> (.subscriptionServiceRelatedData) of
          Just (RentedVehicleNumber t) -> Just t
          _ -> Nothing,
        merchantOperatingCityId = transporterConfig.merchantOperatingCityId,
        startTime = fromMaybe startTime startTime',
        endTime = fromMaybe endTime endTime',
        refundEntityId = Nothing,
        refundedAmount = Nothing,
        refundedAt = Nothing,
        refundedBy = Nothing,
        vehicleCategory = fromMaybe DVC.AUTO_CATEGORY currentVehicleCategory,
        hasSibling = Just False,
        siblingFeeId = Nothing,
        splitOfDriverFeeId = Nothing,
        validDays = Nothing,
        cancellationPenaltyAmount = Nothing,
        addedToFeeId = Nothing,
        collectedAtVendorId = Nothing,
        ..
      }
  where
    specialZoneMetricsIntialization totalFee' = do
      if isSpecialZoneCharge then (1, totalFee') else (0, 0)

getPlan ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe DriverPlan ->
  ServiceNames ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  m (Maybe Plan)
getPlan mbDriverPlan serviceName merchantOpCityId recalculateManualReview mbVehicleCategory = do
  case mbDriverPlan of
    Just dp -> do
      let planType = if fromMaybe False recalculateManualReview then MANUAL else dp.planType
      CQP.findByIdAndPaymentModeWithServiceName dp.planId planType serviceName
    Nothing -> do
      plans <- maybe (pure []) (\vc -> CQP.findByMerchantOpCityIdAndTypeWithServiceName merchantOpCityId DEFAULT serviceName vc False) mbVehicleCategory
      case plans of
        [] -> pure Nothing
        [pl] -> pure (Just pl)
        _ -> throwError $ InternalError "Multiple default plans found"

getDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName)

mkDriverFeeCalcJobCacheKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculation:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

mkDriverFeeCalcJobFlagKey :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = "DriverFeeCalculationFlag:MerchantOpCityId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime <> ":ServiceName:" <> show serviceName

getDriverFeeCalcJobFlagKey :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> m (Maybe Bool)
getDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName = Hedis.get (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName)

setDriverFeeCalcJobCache :: CacheFlow m r => UTCTime -> UTCTime -> Id MerchantOperatingCity -> ServiceNames -> NominalDiffTime -> m ()
setDriverFeeCalcJobCache startTime endTime merchantOpCityId serviceName expTime = do
  Hedis.setExp (mkDriverFeeCalcJobFlagKey startTime endTime merchantOpCityId serviceName) True (round $ expTime + 86399)
  Hedis.setExp (mkDriverFeeCalcJobCacheKey startTime endTime merchantOpCityId serviceName) False (round $ expTime + 86399)

mkDriverFeeBillNumberKey :: Id MerchantOperatingCity -> ServiceNames -> Text -> Text
mkDriverFeeBillNumberKey merchantOpCityId service dateStr = "DriverFeeCalulation:BillNumber:Counter" <> merchantOpCityId.getId <> ":service:" <> show service <> ":" <> dateStr

getDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> ServiceNames -> Text -> m (Maybe Int)
getDriverFeeBillNumberKey merchantOpCityId serviceName dateStr = Hedis.get (mkDriverFeeBillNumberKey merchantOpCityId serviceName dateStr)

mkLockKeyForDriverFeeCalculation :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> Text
mkLockKeyForDriverFeeCalculation startTime endTime merchantOpCityId = "DriverFeeCalculation:Lock:MerchantId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime
