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
    setDriverFeeBillNumberKey,
    getDriverFeeCalcJobCache,
    setDriverFeeCalcJobCache,
    getStartDateMonth,
    getEndDateMonth,
    makeDriverLeaderBoardKey,
    getMonth,
    pickedWaypointsForEditDestination,
    pickNWayPoints,
    makeWalletRunningBalanceLockKey,
    makeSubscriptionRunningBalanceLockKey,
  )
where

import qualified Data.List as DL
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.UI.Plan as Plan
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CancellationCharges as DCC
import qualified Domain.Types.ConditionalCharges as DAC
import Domain.Types.DailyStats as DDS
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.DriverWallet as DW
import Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Domain.Types.Plan
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideRelatedNotificationConfig as DRN
import qualified Domain.Types.RiderDetails as RD
import Domain.Types.SubscriptionConfig as DSC
import qualified Domain.Types.SubscriptionTransaction as SubscriptionTransaction
import Domain.Types.TransporterConfig
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as Variant
import qualified Domain.Types.VendorFee as DVF
import EulerHS.Prelude hiding (elem, foldr, id, length, map, mapM_, null)
import GHC.Float (double2Int)
import GHC.Num.Integer (integerFromInt, integerToInt)
import Kernel.External.Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Payment.Juspay.Types.Common
import Kernel.Prelude hiding (find, forM_, map, whenJust)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common hiding (getCurrentTime)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler.Environment (JobCreatorEnv)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.Scheduler.Types (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import Lib.Types.SpecialLocation hiding (Merchant, MerchantOperatingCity)
import SharedLogic.Allocator
import qualified SharedLogic.Analytics as Analytics
import SharedLogic.DriverOnboarding
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.FleetVehicleStats as FVS
import SharedLogic.Ride (makeSubscriptionRunningBalanceLockKey, multipleRouteKey, searchRequestKey, updateOnRideStatusWithAdvancedRideCheck)
import qualified SharedLogic.ScheduledNotifications as SN
import SharedLogic.TollsDetector
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.PlanExtra as CQP
import qualified Storage.CachedQueries.RideRelatedNotificationConfig as CRN
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.CachedQueries.VendorSplitDetails as CQVSD
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CancellationCharges as QCC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverInformationExtra as QDIE
import Storage.Queries.DriverPlan (findByDriverIdWithServiceName)
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.DriverWallet as QDW
import qualified Storage.Queries.DriverWalletExtra as QDWE
import qualified Storage.Queries.FareParameters as QFare
import Storage.Queries.FleetDriverAssociationExtra as QFDAE
import Storage.Queries.FleetOwnerInformation as QFOI
import Storage.Queries.Person as SQP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.SubscriptionTransaction as QSubscriptionTransaction
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.VendorFee as QVF
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import Tools.Notifications
import qualified Tools.PaymentNudge as PaymentNudge
import Tools.Utils
import Utils.Common.Cac.KeyNameConstants

endRideTransaction ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
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
    HasField "blackListedJobs" r [Text]
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
  merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  updateOnRideStatusWithAdvancedRideCheck ride.driverId (Just ride)
  oldDriverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let newFlowStatus = DDriverMode.getDriverFlowStatus oldDriverInfo.mode oldDriverInfo.active
  DDriverMode.updateDriverModeAndFlowStatus driverId thresholdConfig oldDriverInfo.active Nothing newFlowStatus oldDriverInfo (Just False) Nothing
  let driverInfo = oldDriverInfo {DI.driverFlowStatus = Just newFlowStatus}
  QRB.updateStatus booking.id SRB.COMPLETED
  whenJust mbRiderDetailsId $ \riderDetailsId -> do
    QRiderDetails.updateCompletedRidesCount riderDetailsId.getId
  whenJust mbFareParams QFare.create
  QRide.updateAll ride.id ride
  let safetyPlusCharges = maybe Nothing (\a -> find (\ac -> ac.chargeCategory == DAC.SAFETY_PLUS_CHARGES) a) $ (mbFareParams <&> (.conditionalCharges)) <|> (Just newFareParams.conditionalCharges)
  void $ QDriverStats.incrementTotalRidesAndTotalDistAndIdleTime (cast ride.driverId) (fromMaybe 0 ride.chargeableDistance)
  when thresholdConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    fork "updateFleetVehicleDailyStats and updateOperatorAnalyticsTotalRideCount" $ do
      Analytics.updateOperatorAnalyticsTotalRideCount thresholdConfig driverId ride booking
      whenJust ride.fleetOwnerId $ \fleetOwnerId -> do
        FVS.updateFleetVehicleDailyStats fleetOwnerId.getId thresholdConfig ride
  when (isJust safetyPlusCharges) $ QDriverStats.incSafetyPlusRiderCountAndEarnings (cast ride.driverId) (fromMaybe 0.0 $ safetyPlusCharges <&> (.charge))
  Hedis.del $ multipleRouteKey booking.transactionId
  Hedis.del $ searchRequestKey booking.transactionId
  clearCachedFarePolicyByEstOrQuoteId booking.quoteId
  clearTollStartGateBatchCache ride.driverId
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
      -- QRD.updateDisputeChancesUsedAndCancellationDues (max 0 (riderDetails.disputeChancesUsed - calDisputeChances)) 0 (riderDetails.id) >> QCC.create cancellationCharges
      _ -> logWarning $ "Unable to update customer cancellation dues as RiderDetailsId is NULL with rideId " <> ride.id.getId
  when (fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled) $ do
    case ride.fare of
      Just fare -> fork "update prepaid balance" $ updateBalance fare
      Nothing -> logWarning $ "Fare is not present for ride: " <> show ride.id.getId
  when (thresholdConfig.subscription) $ do
    -- Turn this off for only prepaid subscriptions
    let serviceName = YATRI_SUBSCRIPTION
    createDriverFee booking.providerId booking.merchantOperatingCityId driverId ride.fare ride.currency newFareParams driverInfo booking serviceName
  when (fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled && thresholdConfig.driverWalletConfig.enableDriverWallet) $ do
    fork "createDriverWalletTransaction" $ createDriverWalletTransaction ride booking thresholdConfig

  triggerRideEndEvent RideEventData {ride = ride{status = Ride.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}, personId = cast driverId, merchantId = booking.providerId}

  let validRide = isValidRide ride
  sendReferralFCM validRide ride booking mbRiderDetails thresholdConfig
  when (validRide && (ride.traveledDistance > 1000 || (fromMaybe False ride.distanceCalculationFailed && fromMaybe 0 ride.chargeableDistance > 1000))) $ updateLeaderboardZScore booking ride
  DS.driverScoreEventHandler booking.merchantOperatingCityId DST.OnRideCompletion {merchantId = booking.providerId, driverId = cast driverId, ride = ride, fareParameter = Just newFareParams, ..}
  now <- getCurrentTime
  rideRelatedNotificationConfigList <- CRN.findAllByMerchantOperatingCityIdAndTimeDiffEventInRideFlow booking.merchantOperatingCityId DRN.END_TIME booking.configInExperimentVersions
  forM_ rideRelatedNotificationConfigList (SN.pushReminderUpdatesInScheduler booking ride now driverId)
  where
    updateBalance fare = do
      let subscriptionConfig = thresholdConfig.subscriptionConfig
      case ride.fleetOwnerId of
        Just fleetOwnerId -> do
          when (isJust subscriptionConfig.fleetPrepaidSubscriptionThreshold) $ do
            Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey fleetOwnerId.getId) 10 10 $ do
              fleetOwnerInfo <- QFOI.findByPrimaryKey fleetOwnerId >>= fromMaybeM (PersonNotFound fleetOwnerId.getId)
              let newBalance = fromMaybe 0 fleetOwnerInfo.prepaidSubscriptionBalance - fare
                  reserved = booking.estimatedFare
                  newlien = fromMaybe 0 fleetOwnerInfo.lienAmount - reserved
              QFOI.updatePrepaidSubscriptionBalanceAndLienAmount (Just newBalance) (Just newlien) fleetOwnerId
              createSubscriptionTransaction ride newBalance booking
        Nothing -> do
          when (isJust subscriptionConfig.prepaidSubscriptionThreshold) $ do
            Redis.withWaitOnLockRedisWithExpiry (makeSubscriptionRunningBalanceLockKey ride.driverId.getId) 10 10 $ do
              -- Fetching again to avoid race conditions
              freshDriverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
              let newBalance = fromMaybe 0 freshDriverInfo.prepaidSubscriptionBalance - fare
              driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
              let balanceUpdateMessage = "Thank you for taking the ride. Your updated subscription balance is Rs." <> show newBalance
                  balanceUpdatedTitle = "Subscription balance updated!"
              sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.PREPAID_BALANCE_UPDATE balanceUpdatedTitle balanceUpdateMessage driver driver.deviceToken
              QDIE.updatePrepaidSubscriptionBalance newBalance (cast ride.driverId)
              createSubscriptionTransaction ride newBalance booking
              let prepaidSubscriptionThreshold = subscriptionConfig.prepaidSubscriptionThreshold
              when (newBalance < fromMaybe 0 prepaidSubscriptionThreshold) $ do
                logInfo $ "Prepaid subscription balance is less than threshold for driver: " <> show driverId.getId
                let unsubscribedMessage = "Your subscription balance is low. Please recharge to get rides"
                    unsubscribedTitle = "Low Balance Alert!"
                sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_UNSUBSCRIBED unsubscribedTitle unsubscribedMessage driver driver.deviceToken

createSubscriptionTransaction ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Ride.Ride ->
  HighPrecMoney ->
  SRB.Booking ->
  m ()
createSubscriptionTransaction ride runningBalance booking = do
  now <- getCurrentTime
  id <- generateGUID
  let transaction =
        SubscriptionTransaction.SubscriptionTransaction
          { id = id,
            merchantId = ride.merchantId,
            merchantOperatingCityId = ride.merchantOperatingCityId,
            driverId = ride.driverId,
            fleetOwnerId = ride.fleetOwnerId,
            entityId = Just ride.id.getId,
            transactionType = SubscriptionTransaction.RIDE,
            amount = fromMaybe 0 ride.fare,
            status = CHARGED,
            runningBalance = runningBalance,
            fromLocationId = Just booking.fromLocation.id,
            toLocationId = (.id) <$> booking.toLocation,
            createdAt = now,
            updatedAt = now
          }
  QSubscriptionTransaction.create transaction

createDriverWalletTransaction ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Ride.Ride ->
  SRB.Booking ->
  TransporterConfig ->
  m ()
createDriverWalletTransaction ride booking transporterConfig = do
  now <- getCurrentTime
  newId <- generateGUID
  let collectionAmount = fromMaybe 0 ride.fare
      gstPercentage = transporterConfig.driverWalletConfig.gstPercentage
      gstDeduction = collectionAmount * (realToFrac gstPercentage / 100)

  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey ride.driverId.getId) 10 10 $ do
    lastTransaction <- QDWE.findLatestByDriverId ride.driverId
    let lastRunningBalance = maybe 0 (.runningBalance) lastTransaction

    (merchantPayable, driverPayable) <- do
      mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId ->
        do
          CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
          >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
      case mbPaymentMethod of
        Nothing -> pure (0, gstDeduction) -- Considering OFFLINE. To be tested
        Just paymentMethod -> do
          case paymentMethod.paymentInstrument of
            Cash -> pure (0, gstDeduction) -- OFFLINE
            BoothOnline -> pure (0, gstDeduction) -- OFFLINE
            _ -> pure (collectionAmount - gstDeduction, 0) -- ONLINE
    let newRunningBalance = lastRunningBalance + merchantPayable - driverPayable
    let driverWallet =
          DW.DriverWallet
            { id = newId,
              merchantId = ride.merchantId,
              merchantOperatingCityId = ride.merchantOperatingCityId,
              driverId = ride.driverId,
              rideId = Just ride.id,
              transactionType = DW.RIDE_TRANSACTION,
              collectionAmount = Just collectionAmount,
              gstDeduction = Just gstDeduction,
              merchantPayable = Just merchantPayable,
              driverPayable = Just driverPayable,
              runningBalance = newRunningBalance,
              payoutOrderId = Nothing,
              payoutStatus = Nothing,
              createdAt = now,
              updatedAt = now
            }
    QDI.updateWalletBalance (Just newRunningBalance) ride.driverId
    QDW.create driverWallet

makeWalletRunningBalanceLockKey :: Text -> Text
makeWalletRunningBalanceLockKey personId = "WalletRunningBalanceLockKey:" <> personId

sendReferralFCM ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Bool ->
  Ride.Ride ->
  SRB.Booking ->
  Maybe RD.RiderDetails ->
  TransporterConfig ->
  m ()
sendReferralFCM validRide ride booking mbRiderDetails transporterConfig = do
  now <- getCurrentTime
  let shouldUpdateRideComplete = validRide && maybe True (not . (.hasTakenValidRide)) mbRiderDetails
  whenJust mbRiderDetails $ \riderDetails -> do
    fork "REFERRAL_ACTIVATED FCM to Driver" $ do
      when shouldUpdateRideComplete $ QRD.updateHasTakenValidRide True (Just now) riderDetails.id
      case riderDetails.referredByDriver of
        Just referredDriverId -> do
          driver <- SQP.findById referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
          when shouldUpdateRideComplete $ do
            let referralMessage = "Congratulations!"
                referralTitle = "Your referred customer has completed their first Namma Yatri ride"
            sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver driver.deviceToken
            fork "DriverToCustomerReferralCoin Event : " $ do
              DC.driverCoinsEvent driver.id driver.merchantId driver.merchantOperatingCityId (DCT.DriverToCustomerReferral ride) (Just ride.id.getId) ride.vehicleVariant (Just booking.configInExperimentVersions)
          mbVehicle <- QV.findById referredDriverId
          let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
          payoutConfig <- CPC.findByPrimaryKey driver.merchantOperatingCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) driver.merchantOperatingCityId.getId)
          when (isNothing riderDetails.firstRideId && payoutConfig.isPayoutEnabled) $ do
            let mobileNumberHash = (.hash) riderDetails.mobileNumber
            localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
            mbDailyStats <- QDailyStats.findByDriverIdAndDate referredDriverId (utctDay localTime)
            (isValidRideForPayout, mbFlagReason) <- fraudChecksForReferralPayout mobileNumberHash riderDetails mbDailyStats
            QRD.updateFirstRideIdAndFlagReason (Just ride.id.getId) mbFlagReason riderDetails.id
            when (isValidRideForPayout && isConsideredForPayout payoutConfig riderDetails) $ fork "Updating Payout Stats of Driver : " $ updateReferralStats referredDriverId mbDailyStats localTime driver driver.merchantOperatingCityId payoutConfig
        Nothing -> pure ()
  where
    isConsideredForPayout payoutConfig riderDetails = do
      let programStartDate = fromMaybe getDefaultTime payoutConfig.referralProgramStartDate
      maybe False (\referredAt -> referredAt >= programStartDate) riderDetails.referredAt

    updateReferralStats referredDriverId mbDailyStats localTime driver merchantOpCityId payoutConfig = do
      driverInfo <- QDI.findById (cast referredDriverId) >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      when (isNothing driverInfo.payoutVpa) do
        mbMerchantPN_ <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_VPA_ALERT" Nothing Nothing driver.language Nothing
        whenJust mbMerchantPN_ $ \merchantPN_ -> do
          let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN_.title
              entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN_.body}
          notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN_.fcmNotificationType -- Sending PN to Add Vpa
      mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_REFERRAL_REWARD" Nothing Nothing driver.language Nothing
      whenJust mbMerchantPN $ \merchantPN -> do
        let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN.title
            entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN.body}
        notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType -- Sending PN for Reward
      let referralRewardAmount = payoutConfig.referralRewardAmountPerRide
      driverStats <- QDriverStats.findByPrimaryKey referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      QDriverStats.updateTotalValidRidesAndPayoutEarnings (driverStats.totalValidActivatedRides + 1) (driverStats.totalPayoutEarnings + referralRewardAmount) referredDriverId
      case mbDailyStats of
        Just stats -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey referredDriverId.getId) 3 3 $ do
            QDailyStats.updateReferralStatsByDriverId (stats.activatedValidRides + 1) (stats.referralEarnings + referralRewardAmount) DDS.Verifying referredDriverId (utctDay localTime)
        Nothing -> do
          id <- generateGUIDText
          now <- getCurrentTime
          let dailyStatsOfDriver' =
                DDS.DailyStats
                  { id = id,
                    driverId = referredDriverId,
                    totalEarnings = 0.0,
                    numRides = 0,
                    totalDistance = 0,
                    tollCharges = 0.0,
                    bonusEarnings = 0.0,
                    merchantLocalDate = utctDay localTime,
                    currency = ride.currency,
                    distanceUnit = ride.distanceUnit,
                    activatedValidRides = 1,
                    referralEarnings = referralRewardAmount,
                    referralCounts = 1,
                    payoutStatus = DDS.Verifying,
                    payoutOrderId = Nothing,
                    payoutOrderStatus = Nothing,
                    createdAt = now,
                    updatedAt = now,
                    cancellationCharges = 0.0,
                    tipAmount = 0.0,
                    totalRideTime = 0,
                    numDriversOnboarded = 0,
                    numFleetsOnboarded = 0,
                    merchantId = ride.merchantId,
                    merchantOperatingCityId = Just $ ride.merchantOperatingCityId,
                    onlineDuration = Nothing
                  }
          QDailyStats.create dailyStatsOfDriver'

    fraudChecksForReferralPayout mobileNumberHash riderDetails mbDailyStats = do
      availablePersonWithNumber <- SQP.findAllMerchantIdByPhoneNo riderDetails.mobileCountryCode mobileNumberHash
      let isMaxReferralExceeded = maybe True ((<= transporterConfig.maxPayoutReferralForADay) . (.activatedValidRides)) mbDailyStats
          isMultipleDeviceIdExists = isJust riderDetails.payoutFlagReason
      let mbFlagReason =
            case (listToMaybe availablePersonWithNumber, validRide, isMaxReferralExceeded) of
              (Just _, _, _) -> Just RD.CustomerExistAsDriver
              (_, False, _) -> Just RD.RideConstraintInvalid
              (_, _, False) -> Just RD.ExceededMaxReferral
              _ -> riderDetails.payoutFlagReason
      let isValid = null availablePersonWithNumber && validRide && not isMultipleDeviceIdExists
      return (isValid, mbFlagReason)

    payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

getDefaultTime :: UTCTime
getDefaultTime = defaultTime
  where
    day = fromGregorian 2024 7 26
    time = secondsToDiffTime 0
    defaultTime = UTCTime day time

updateLeaderboardZScore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => SRB.Booking -> Ride.Ride -> m ()
updateLeaderboardZScore booking ride = do
  fork "Updating ZScore for driver" . Hedis.withNonCriticalRedis $ mapM_ updateLeaderboardZScore' [LConfig.DAILY, LConfig.WEEKLY, LConfig.MONTHLY]
  where
    updateLeaderboardZScore' :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => LConfig.LeaderBoardType -> m ()
    updateLeaderboardZScore' leaderBoardType = do
      currentTime <- getCurrentTime
      leaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyTypeInRideFlow leaderBoardType booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (InternalError "Leaderboard configs not present")
      when leaderBoardConfig.isEnabled $ do
        let rideDate = getCurrentDate currentTime
            (fromDate, toDate) = calculateFromDateToDate leaderBoardType rideDate
            leaderBoardKey = makeDriverLeaderBoardKey leaderBoardType False booking.merchantOperatingCityId fromDate toDate
        driverZscore <- Hedis.zScore leaderBoardKey $ ride.driverId.getId
        updateDriverZscore ride rideDate fromDate toDate driverZscore ride.chargeableDistance booking.providerId booking.merchantOperatingCityId leaderBoardConfig

    calculateFromDateToDate :: LConfig.LeaderBoardType -> Day -> (Day, Day)
    calculateFromDateToDate leaderBoardType rideDate =
      case leaderBoardType of
        LConfig.DAILY -> (rideDate, rideDate)
        LConfig.WEEKLY ->
          let (_, currDayIndex) = sundayStartWeek rideDate
              weekStartDate = addDays (fromIntegral (- currDayIndex)) rideDate
              weekEndDate = addDays (fromIntegral (6 - currDayIndex)) rideDate
           in (weekStartDate, weekEndDate)
        LConfig.MONTHLY ->
          let monthStartDate = getStartDateMonth rideDate
              monthEndDate = getEndDateMonth rideDate 1
           in (monthStartDate, monthEndDate)

updateDriverZscore :: (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) => Ride.Ride -> Day -> Day -> Day -> Maybe Double -> Maybe Meters -> Id Merchant -> Id DMOC.MerchantOperatingCity -> LConfig.LeaderBoardConfigs -> m ()
updateDriverZscore ride rideDate fromDate toDate driverZscore rideChargeableDistance _ merchantOpCityId leaderBoardConfig = do
  (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
  let leaderBoardExpiry = calculateLeaderBoardExpiry - secondsFromTimeOfDay localTime
      driverLeaderBoardKey = makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType False merchantOpCityId fromDate toDate
      cachedDriverLeaderBoardKey = makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType True merchantOpCityId fromDate toDate
  Hedis.zAddExp driverLeaderBoardKey ride.driverId.getId calculateCurrentZscore leaderBoardExpiry.getSeconds
  let limit = integerFromInt leaderBoardConfig.leaderBoardLengthLimit
  driversListWithScores' <- Hedis.zrevrangeWithscores driverLeaderBoardKey 0 (limit - 1)
  Hedis.setExp cachedDriverLeaderBoardKey driversListWithScores' calculateTotalExpiry.getSeconds
  where
    calculateLeaderBoardExpiry :: Seconds
    calculateLeaderBoardExpiry = do
      case leaderBoardConfig.leaderBoardType of
        LConfig.DAILY -> dailyExpiry
        LConfig.WEEKLY -> weeklyExpiry
        LConfig.MONTHLY -> monthlyExpiry
      where
        dailyExpiry = leaderBoardConfig.leaderBoardExpiry
        weeklyExpiry = do
          let (_, currDayIndex) = sundayStartWeek rideDate
          leaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + Seconds 86400
        monthlyExpiry = Seconds $ integerToInt $ diffDays toDate rideDate * 86400 + 86400

    calculateCurrentZscore :: Integer
    calculateCurrentZscore =
      fromIntegral $ case driverZscore of
        Nothing -> leaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
        Just zscore -> do
          let (prevTotalRides, prevTotalDistance) = getRidesAndDistancefromZscore zscore leaderBoardConfig.zScoreBase
          let currTotalRides = prevTotalRides + 1
          let currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
          currTotalRides * leaderBoardConfig.zScoreBase + getMeters currTotalDist

    calculateTotalExpiry :: Seconds
    calculateTotalExpiry =
      case leaderBoardConfig.leaderBoardType of
        LConfig.MONTHLY -> Seconds $ integerToInt $ diffDays (getEndDateMonth rideDate leaderBoardConfig.numberOfSets) fromDate * 86400
        _ -> Seconds $ leaderBoardConfig.leaderBoardExpiry.getSeconds * leaderBoardConfig.numberOfSets

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

getStartDateMonth :: Day -> Day
getStartDateMonth day = fromGregorian y m 1
  where
    (y, m, _) = toGregorian day

getMonth :: Day -> Int
getMonth = (\(_, m, _) -> m) . toGregorian

getEndDateMonth :: Day -> Int -> Day
getEndDateMonth day addMonths = pred $ addGregorianMonthsClip (integerFromInt addMonths) $ getStartDateMonth day

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
    HasKafkaProducer r
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
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
      vehicle <- QV.findById driverId
      let currentVehicleCategory = vehicle >>= (.category)
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
        numRides <- case lastDriverFee of
          Just ldFee ->
            if now >= ldFee.startTime && now < ldFee.endTime
              then do
                QDF.updateFee ldFee.id rideFare govtCharges platformFee cgst sgst now True booking isSpecialZoneCharge
                return (ldFee.numRides + 1)
              else do
                createWithMbSibling driverFee lastElderSiblingDriverFee ldFee
                return 1
          Nothing -> do
            createWithMbSibling driverFee lastElderSiblingDriverFee driverFee
            return 1
        fork "Updating vendor fees" $
          when (fromMaybe False (subscriptionConfig >>= (.isVendorSplitEnabled))) $ do
            let vehicleVariant = Variant.castServiceTierToVariant booking.vehicleServiceTier
            allVendorSplitDetails <- CQVSD.findAllByAreaIncludingDefaultAndCityAndVariant booking.area merchantOpCityId vehicleVariant
            let vendorSplitDetails = case booking.area of
                  Just area ->
                    let areaDetails = DL.filter (\detail -> detail.area == area) allVendorSplitDetails
                     in if null areaDetails
                          then DL.filter (\detail -> detail.area == Default) allVendorSplitDetails
                          else areaDetails
                  Nothing -> DL.filter (\detail -> detail.area == Default) allVendorSplitDetails
            unless (null vendorSplitDetails) $ do
              let vendorData = DL.map (\vendor -> (vendor.vendorId, toRational vendor.splitValue, vendor.maxVendorFeeAmount)) vendorSplitDetails
                  -- Pass vendor fee along with its maxVendorFeeAmount limit for cumulative validation
                  vendorFeesWithLimit = DL.map (\(vendorId, amount, maxLimit) -> (mkVendorFee (maybe driverFee.id (.id) lastDriverFee) now (vendorId, amount, maxLimit), maxLimit)) vendorData
              unless (null vendorFeesWithLimit) $ do
                case lastDriverFee of
                  Just ldFee | now >= ldFee.startTime && now < ldFee.endTime -> QVF.updateManyVendorFeeWithMaxLimit merchantOpCityId vendorFeesWithLimit
                  _ -> QVF.createManyWithMaxLimit vendorFeesWithLimit

        plan <- getPlan mbDriverPlan serviceName merchantOpCityId Nothing currentVehicleCategory
        fork "Sending switch plan nudge" $ PaymentNudge.sendSwitchPlanNudge transporterConfig driverInfo plan mbDriverPlan numRides serviceName
        scheduleJobs transporterConfig driverFee merchantId merchantOpCityId now
    mkVendorFee driverFeeId now (vendorId, amount, _) = DVF.VendorFee {amount = HighPrecMoney amount, driverFeeId = driverFeeId, vendorId = vendorId, createdAt = now, updatedAt = now}
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

scheduleJobs :: (CacheFlow m r, EsqDBFlow m r, JobCreatorEnv r, HasField "schedulerType" r SchedulerType) => TransporterConfig -> DF.DriverFee -> Id Merchant -> Id MerchantOperatingCity -> UTCTime -> m ()
scheduleJobs transporterConfig driverFee merchantId merchantOpCityId now = do
  void $
    case transporterConfig.driverFeeCalculationTime of
      Nothing -> pure ()
      Just dfCalcTime -> do
        whenWithLockRedis (mkLockKeyForDriverFeeCalculation driverFee.startTime driverFee.endTime merchantOpCityId) 60 $ do
          isDfCaclculationJobScheduled <- getDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName
          let dfCalculationJobTs = diffUTCTime (addUTCTime dfCalcTime driverFee.endTime) now
          case isDfCaclculationJobScheduled of
            ----- marker ---
            Nothing -> do
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
                    endTime = driverFee.endTime
                  }
              setDriverFeeCalcJobCache driverFee.startTime driverFee.endTime merchantOpCityId driverFee.serviceName dfCalculationJobTs
              setDriverFeeBillNumberKey merchantOpCityId 1 36000 (driverFee.serviceName)
            _ -> pure ()

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
        createdAt = now,
        updatedAt = now,
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

mkDriverFeeBillNumberKey :: Id MerchantOperatingCity -> ServiceNames -> Text
mkDriverFeeBillNumberKey merchantOpCityId service = "DriverFeeCalulation:BillNumber:Counter" <> merchantOpCityId.getId <> ":service:" <> show service

getDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> ServiceNames -> m (Maybe Int)
getDriverFeeBillNumberKey merchantOpCityId serviceName = Hedis.get (mkDriverFeeBillNumberKey merchantOpCityId serviceName)

setDriverFeeBillNumberKey :: CacheFlow m r => Id MerchantOperatingCity -> Int -> NominalDiffTime -> ServiceNames -> m ()
setDriverFeeBillNumberKey merchantOpCityId count expTime serviceName = Hedis.setExp (mkDriverFeeBillNumberKey merchantOpCityId serviceName) count (round expTime)

mkLockKeyForDriverFeeCalculation :: UTCTime -> UTCTime -> Id MerchantOperatingCity -> Text
mkLockKeyForDriverFeeCalculation startTime endTime merchantOpCityId = "DriverFeeCalculation:Lock:MerchantId:" <> merchantOpCityId.getId <> ":StartTime:" <> show startTime <> ":EndTime:" <> show endTime
