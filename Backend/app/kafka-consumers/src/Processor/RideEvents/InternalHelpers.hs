-- | Helpers moved out of `Domain.Action.UI.Ride.EndRide.Internal` because their
-- only callers are the consumer-side handlers in `Processor.RideEvents.Handlers`.
-- Driver-app no longer needs to know about referral FCM, fraud checks, wallet
-- credits, or leaderboard zscore updates.
module Processor.RideEvents.InternalHelpers
  ( sendReferralFCM,
    sendDriverToDriverReferralReward,
    creditReferralWallet,
    fraudChecksForReferralPayout,
    updateLeaderboardZScore,
    updateDriverZscore,
    getStartDateMonth,
    getEndDateMonth,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Text as T
import Data.Time
  ( Day,
    LocalTime (..),
    UTCTime (UTCTime),
    addDays,
    addGregorianMonthsClip,
    diffDays,
    fromGregorian,
    secondsToDiffTime,
    toGregorian,
    utcToLocalTime,
    utctDay,
  )
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Ride.EndRide.Internal as EndRideInt
import qualified "dynamic-offer-driver-app" Domain.Types.Booking as SRB
import qualified "dynamic-offer-driver-app" Domain.Types.DailyStats as DDS
import qualified "dynamic-offer-driver-app" Domain.Types.LeaderBoardConfigs as LConfig
import "dynamic-offer-driver-app" Domain.Types.Merchant (Merchant)
import qualified "dynamic-offer-driver-app" Domain.Types.MerchantOperatingCity as DMOC
import qualified "dynamic-offer-driver-app" Domain.Types.PayoutConfig as DPC
import qualified "dynamic-offer-driver-app" Domain.Types.Person as DP
import qualified "dynamic-offer-driver-app" Domain.Types.Ride as Ride
import qualified "dynamic-offer-driver-app" Domain.Types.RiderDetails as RD
import "dynamic-offer-driver-app" Domain.Types.TransporterConfig (TransporterConfig)
import qualified "beckn-spec" Domain.Types.VehicleCategory as DVC
import GHC.Num.Integer (integerFromInt, integerToInt)
import Kernel.External.Encryption (DbHash, EncFlow)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CHConfig
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto.Config as Esq
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (Currency, HighPrecMoney, Meters (..), MonadFlow, Seconds (..), getMeters)
import Kernel.Types.Id
import Kernel.Utils.Common
  ( CacheFlow,
    Forkable (fork),
    fromMaybeM,
    generateGUIDText,
    getCurrentTime,
    getLocalCurrentTime,
    logError,
  )
import Kernel.Utils.Time (secondsFromTimeOfDay)
import qualified "dynamic-offer-driver-app" Lib.DriverCoins.Coins as DC
import qualified "dynamic-offer-driver-app" Lib.DriverCoins.Types as DCT
import Lib.Finance.FinanceEvents.Publisher (FinanceEventsPublisherCfg)
import "dynamic-offer-driver-app" SharedLogic.FareCalculator (timeZoneIST)
import "dynamic-offer-driver-app" SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import "dynamic-offer-driver-app" SharedLogic.Finance.Wallet (createWalletEntryDelta, walletReferenceD2DReferral)
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified "dynamic-offer-driver-app" Storage.Queries.DailyStats as QDailyStats
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverInformation as QDI
import qualified "dynamic-offer-driver-app" Storage.Queries.DriverStats as QDriverStats
import qualified "dynamic-offer-driver-app" Storage.Queries.FleetDriverAssociationExtra as QFDAE
import qualified "dynamic-offer-driver-app" Storage.Queries.Person as SQP
import qualified "dynamic-offer-driver-app" Storage.Queries.Ride as QRide
import qualified "dynamic-offer-driver-app" Storage.Queries.RiderDetails as QRD
import qualified "dynamic-offer-driver-app" Storage.Queries.Vehicle as QV
import "dynamic-offer-driver-app" Tools.Error
import "dynamic-offer-driver-app" Tools.Notifications (NotifReq (..), notifyDriverOnEvents, sendNotificationToDriver)

------------------------------------------------------------
-- Default reference time
------------------------------------------------------------

getDefaultTime :: UTCTime
getDefaultTime = UTCTime (fromGregorian 2024 7 26) (secondsToDiffTime 0)

------------------------------------------------------------
-- Referral FCM (customer-to-driver referral)
------------------------------------------------------------

sendReferralFCM ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    CHConfig.ClickhouseFlow m r,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    HasField "financeEventsPublisherCfg" r (Maybe FinanceEventsPublisherCfg)
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
                referralTitle = "Your referred customer has completed their first ride"
            sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage driver driver.deviceToken
            fork "DriverToCustomerReferralCoin Event : " $ do
              DC.driverCoinsEvent driver.id Nothing driver.merchantId driver.merchantOperatingCityId (DCT.DriverToCustomerReferral ride) (Just ride.id.getId) ride.vehicleVariant (Just booking.vehicleServiceTier) (Just booking.configInExperimentVersions)
          mbVehicle <- QV.findById referredDriverId
          let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
          payoutConfig <- CPC.findByPrimaryKey driver.merchantOperatingCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) driver.merchantOperatingCityId.getId)
          when (isNothing riderDetails.firstRideId && payoutConfig.isPayoutEnabled) $ do
            let mobileNumberHash = (.hash) riderDetails.mobileNumber
            localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
            mbDailyStats <- QDailyStats.findByDriverIdAndDate referredDriverId (utctDay localTime)
            (isValidRideForPayout, mbFlagReason) <- fraudChecksForReferralPayout validRide transporterConfig mobileNumberHash riderDetails mbDailyStats
            QRD.updateFirstRideIdAndFlagReason (Just ride.id.getId) mbFlagReason riderDetails.id
            when (isValidRideForPayout && isConsideredForPayout payoutConfig riderDetails) $ fork "Updating Payout Stats of Driver : " $ updateReferralStats referredDriverId mbDailyStats localTime driver driver.merchantOperatingCityId payoutConfig driver
        Nothing -> pure ()
  where
    isConsideredForPayout payoutConfig riderDetails = do
      let programStartDate = fromMaybe getDefaultTime payoutConfig.referralProgramStartDate
      maybe False (\referredAt -> referredAt >= programStartDate) riderDetails.referredAt

    updateReferralStats referredDriverId mbDailyStats localTime driver merchantOpCityId payoutConfig referredDriver = do
      driverInfo <- QDI.findById (cast referredDriverId) >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      when (isNothing driverInfo.payoutVpa) do
        mbMerchantPN_ <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_VPA_ALERT" Nothing Nothing driver.language Nothing
        whenJust mbMerchantPN_ $ \merchantPN_ -> do
          let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN_.title
              entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN_.body}
          notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN_.fcmNotificationType
      mbMerchantPN <- CPN.findMatchingMerchantPN merchantOpCityId "PAYOUT_REFERRAL_REWARD" Nothing Nothing driver.language Nothing
      whenJust mbMerchantPN $ \merchantPN -> do
        let title = T.replace "{#rewardAmount#}" (show payoutConfig.referralRewardAmountPerRide) merchantPN.title
            entityData = NotifReq {entityId = referredDriverId.getId, title = title, message = merchantPN.body}
        notifyDriverOnEvents merchantOpCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
      let baseReferralAmount = payoutConfig.referralRewardAmountPerRide
          (deltaReferralEarnings, newPayoutStatus) =
            case payoutConfig.d2dPayoutType of
              DPC.NO_PAYOUT -> (0.0, DDS.Success)
              DPC.DIRECT_PAYOUT -> (baseReferralAmount, DDS.Verifying)
              DPC.WALLET -> (baseReferralAmount, DDS.Success)

      driverStats <- QDriverStats.findByPrimaryKey referredDriverId >>= fromMaybeM (PersonNotFound referredDriverId.getId)
      QDriverStats.updateTotalValidRidesAndPayoutEarnings (driverStats.totalValidActivatedRides + 1) (driverStats.totalPayoutEarnings + deltaReferralEarnings) referredDriverId

      case mbDailyStats of
        Just stats -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey referredDriverId.getId) 3 3 $ do
            QDailyStats.updateReferralStatsByDriverId (stats.activatedValidRides + 1) (stats.referralEarnings + deltaReferralEarnings) newPayoutStatus referredDriverId (utctDay localTime)
          when (payoutConfig.d2dPayoutType == DPC.WALLET) $
            creditReferralWallet deltaReferralEarnings referredDriverId stats.id "d2cReferralEarnings" ride.currency referredDriver.merchantId.getId referredDriver.merchantOperatingCityId.getId
        Nothing -> do
          newId <- generateGUIDText
          now <- getCurrentTime
          let dailyStatsOfDriver' =
                DDS.DailyStats
                  { id = newId,
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
                    referralEarnings = deltaReferralEarnings,
                    referralCounts = 1,
                    d2dReferralEarnings = 0.0,
                    d2dReferralCounts = 0,
                    d2dActivatedValidRides = 0,
                    payoutStatus = newPayoutStatus,
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
          when (payoutConfig.d2dPayoutType == DPC.WALLET) $
            creditReferralWallet deltaReferralEarnings referredDriverId newId "d2cReferralEarnings" ride.currency referredDriver.merchantId.getId referredDriver.merchantOperatingCityId.getId

    payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

------------------------------------------------------------
-- Shared fraud checks for referral payout
------------------------------------------------------------

fraudChecksForReferralPayout ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r
  ) =>
  Bool ->
  TransporterConfig ->
  DbHash ->
  RD.RiderDetails ->
  Maybe DDS.DailyStats ->
  m (Bool, Maybe RD.PayoutFlagReason)
fraudChecksForReferralPayout validRide transporterConfig mobileNumberHash riderDetails mbDailyStats = do
  availablePersonWithNumber <- SQP.findAllMerchantIdByPhoneNo riderDetails.mobileCountryCode mobileNumberHash
  let totalPayoutCount = maybe 0 (\s -> s.referralCounts + s.d2dReferralCounts) mbDailyStats
      isMaxReferralExceeded = totalPayoutCount <= transporterConfig.maxPayoutReferralForADay
      isMultipleDeviceIdExists = isJust riderDetails.payoutFlagReason
  let mbFlagReason =
        case (listToMaybe availablePersonWithNumber, validRide, isMaxReferralExceeded) of
          (Just _, _, _) -> Just RD.CustomerExistAsDriver
          (_, False, _) -> Just RD.RideConstraintInvalid
          (_, _, False) -> Just RD.ExceededMaxReferral
          _ -> riderDetails.payoutFlagReason
  let isValid = null availablePersonWithNumber && validRide && not isMultipleDeviceIdExists
  return (isValid, mbFlagReason)

------------------------------------------------------------
-- Driver-to-driver referral reward
------------------------------------------------------------

sendDriverToDriverReferralReward ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Redis.HedisLTSFlowEnv r,
    Redis.HedisFlow m r,
    HasField "financeEventsPublisherCfg" r (Maybe FinanceEventsPublisherCfg)
  ) =>
  Bool ->
  Ride.Ride ->
  SRB.Booking ->
  Maybe RD.RiderDetails ->
  TransporterConfig ->
  m ()
sendDriverToDriverReferralReward validRide ride _booking mbRiderDetails transporterConfig = do
  driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  referredDriverStats <- QDriverStats.findByPrimaryKey (cast ride.driverId)
  let isFirstRide = (== 1) . (.totalRides) <$> referredDriverStats
  when (validRide && isJust driverInfo.referredByDriverId && (fromMaybe False isFirstRide)) $ do
    let referringDriverId = fromJust driverInfo.referredByDriverId
    referringDriver <- SQP.findById referringDriverId >>= fromMaybeM (PersonNotFound referringDriverId.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    mbDailyStats <- QDailyStats.findByDriverIdAndDate referringDriverId (utctDay localTime)
    passedFraudCheck <-
      case mbRiderDetails of
        Nothing -> pure False
        Just riderDetails -> do
          (isValid, mbFlagReason) <- fraudChecksForReferralPayout validRide transporterConfig ((.hash) riderDetails.mobileNumber) riderDetails mbDailyStats
          QRide.updateReferralFlagReason mbFlagReason ride.id
          pure isValid
    when passedFraudCheck $ do
      let referralMessage = "Congratulations!"
          referralTitle = "Your referred driver has completed their first ride"
      sendNotificationToDriver referringDriver.merchantOperatingCityId FCM.SHOW Nothing FCM.REFERRAL_ACTIVATED referralTitle referralMessage referringDriver referringDriver.deviceToken
      mbVehicle <- QV.findById referringDriverId
      let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
      payoutConfig <- CPC.findByPrimaryKey referringDriver.merchantOperatingCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) referringDriver.merchantOperatingCityId.getId)
      when (payoutConfig.isPayoutEnabled && driverInfo.isBlockedForReferralPayout /= Just True) $ do
        let totalPayoutCount = maybe 0 (\s -> s.referralCounts + s.d2dReferralCounts) mbDailyStats
            isMaxReferralExceeded = totalPayoutCount <= transporterConfig.maxPayoutReferralForADay
        when isMaxReferralExceeded $
          fork "Updating Payout Stats of Referring Driver (driver-to-driver)" $
            updateReferralStatsForDriverToDriver referringDriverId mbDailyStats localTime payoutConfig referringDriver
  where
    updateReferralStatsForDriverToDriver referringDriverId mbDailyStats localTime payoutConfig referringDriver = do
      d2dRewardAmount <-
        case payoutConfig.d2dPayoutType of
          DPC.WALLET -> getD2DRewardAmount payoutConfig
          DPC.DIRECT_PAYOUT -> getD2DRewardAmount payoutConfig
          DPC.NO_PAYOUT -> pure 0.0

      driverStats <- QDriverStats.findByPrimaryKey referringDriverId >>= fromMaybeM (PersonNotFound referringDriverId.getId)

      let (deltaD2dEarnings, newPayoutStatus) =
            case payoutConfig.d2dPayoutType of
              DPC.NO_PAYOUT -> (0.0, DDS.Success)
              DPC.DIRECT_PAYOUT -> (d2dRewardAmount, DDS.Verifying)
              DPC.WALLET -> (d2dRewardAmount, DDS.Success)

      QDriverStats.updateTotalValidRidesAndPayoutEarnings (driverStats.totalValidActivatedRides + 1) (driverStats.totalPayoutEarnings + deltaD2dEarnings) referringDriverId
      QDriverStats.updateD2dReferralCount (driverStats.d2dReferralCount + 1) (driverStats.totalReferralCounts + 1) referringDriverId

      case mbDailyStats of
        Just stats -> do
          Redis.withWaitOnLockRedisWithExpiry (payoutProcessingLockKey referringDriverId.getId) 3 3 $ do
            QDailyStats.updateD2dReferralStatsByDriverId (stats.d2dReferralEarnings + deltaD2dEarnings) (stats.d2dActivatedValidRides + 1) newPayoutStatus referringDriverId (utctDay localTime)
            QDailyStats.updateD2dReferralCount (stats.d2dReferralCounts + 1) referringDriverId (utctDay localTime)

          when (payoutConfig.d2dPayoutType == DPC.WALLET) $
            creditReferralWallet deltaD2dEarnings referringDriverId stats.id "d2dReferralEarnings" ride.currency referringDriver.merchantId.getId referringDriver.merchantOperatingCityId.getId
        Nothing -> do
          newId <- generateGUIDText
          now <- getCurrentTime
          let dailyStatsOfDriver' =
                DDS.DailyStats
                  { id = newId,
                    driverId = referringDriverId,
                    totalEarnings = 0.0,
                    numRides = 0,
                    totalDistance = 0,
                    tollCharges = 0.0,
                    bonusEarnings = 0.0,
                    merchantLocalDate = utctDay localTime,
                    currency = ride.currency,
                    distanceUnit = ride.distanceUnit,
                    activatedValidRides = 0,
                    referralEarnings = 0.0,
                    referralCounts = 0,
                    d2dReferralEarnings = deltaD2dEarnings,
                    d2dReferralCounts = 1,
                    d2dActivatedValidRides = 1,
                    payoutStatus = newPayoutStatus,
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
                    merchantOperatingCityId = Just ride.merchantOperatingCityId,
                    onlineDuration = Nothing
                  }
          QDailyStats.create dailyStatsOfDriver'

          when (payoutConfig.d2dPayoutType == DPC.WALLET) $
            creditReferralWallet deltaD2dEarnings referringDriverId newId "d2dReferralEarnings" ride.currency referringDriver.merchantId.getId referringDriver.merchantOperatingCityId.getId

    getD2DRewardAmount payoutConfig = do
      let mbAmount = payoutConfig.referralRewardAmountPerRideForD2DPayout
      fromMaybeM (InternalError "referralRewardAmountPerRideForD2DPayout not configured for d2d payout") mbAmount

    payoutProcessingLockKey driverId = "Payout:Processing:DriverId" <> driverId

------------------------------------------------------------
-- Wallet credit (shared between sendReferralFCM and sendDriverToDriverReferralReward)
------------------------------------------------------------

creditReferralWallet ::
  ( CacheFlow m r,
    Esq.EsqDBFlow m r,
    MonadFlow m,
    Redis.HedisFlow m r,
    HasField "financeEventsPublisherCfg" r (Maybe FinanceEventsPublisherCfg)
  ) =>
  HighPrecMoney ->
  Id DP.Person ->
  Text ->
  Text ->
  Currency ->
  Text ->
  Text ->
  m ()
creditReferralWallet amount driverId_ dailyStatsId earningsKey currency merchantId merchantOperatingCityId =
  when (amount > 0) $ do
    mbFleetDriverAssociation <- QFDAE.findByDriverId driverId_ True
    let mbFleetOwnerId = (\fda -> Id fda.fleetOwnerId) <$> mbFleetDriverAssociation
    let (counterparty, ownerId) = case mbFleetOwnerId of
          Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId.getId)
          Nothing -> (counterpartyDriver, driverId_.getId)
    let metadata =
          A.object
            [ AKey.fromText earningsKey A..= amount,
              "dailyStatsId" A..= dailyStatsId
            ]
    resp <-
      createWalletEntryDelta
        counterparty
        ownerId
        amount
        currency
        merchantId
        merchantOperatingCityId
        walletReferenceD2DReferral
        dailyStatsId
        (Just metadata)
    case resp of
      Left err -> do
        logError $ "Failed to create referral wallet entry for driverId: " <> driverId_.getId <> " dailyStatsId: " <> dailyStatsId <> " err: " <> show err
        Redis.withWaitOnLockRedisWithExpiry ("Payout:Processing:DriverId" <> driverId_.getId) 3 3 $
          QDailyStats.updatePayoutStatusById DDS.Failed dailyStatsId
      Right _ ->
        pure ()

------------------------------------------------------------
-- Leaderboard zscore
------------------------------------------------------------

updateLeaderboardZScore ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) =>
  SRB.Booking ->
  Ride.Ride ->
  m ()
updateLeaderboardZScore booking ride =
  fork "Updating ZScore for driver" . Hedis.runInMasterCloudRedisCell . Hedis.withNonCriticalRedis $
    mapM_ updateLeaderboardZScore' [LConfig.DAILY, LConfig.WEEKLY, LConfig.MONTHLY]
  where
    updateLeaderboardZScore' ::
      (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) =>
      LConfig.LeaderBoardType ->
      m ()
    updateLeaderboardZScore' leaderBoardType = do
      currentTime <- getCurrentTime
      leaderBoardConfig <-
        QLeaderConfig.findLeaderBoardConfigbyTypeInRideFlow leaderBoardType booking.merchantOperatingCityId booking.configInExperimentVersions
          >>= fromMaybeM (InternalError "Leaderboard configs not present")
      when leaderBoardConfig.isEnabled $ do
        let rideDate = EndRideInt.getCurrentDate currentTime
            (fromDate, toDate) = calculateFromDateToDate leaderBoardType rideDate
            leaderBoardKey = EndRideInt.makeDriverLeaderBoardKey leaderBoardType False booking.merchantOperatingCityId fromDate toDate
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

updateDriverZscore ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) =>
  Ride.Ride ->
  Day ->
  Day ->
  Day ->
  Maybe Double ->
  Maybe Meters ->
  Id Merchant ->
  Id DMOC.MerchantOperatingCity ->
  LConfig.LeaderBoardConfigs ->
  m ()
updateDriverZscore ride rideDate fromDate toDate driverZscore rideChargeableDistance _ merchantOpCityId leaderBoardConfig = do
  (LocalTime _ localTime) <- utcToLocalTime timeZoneIST <$> getCurrentTime
  let leaderBoardExpiry = calculateLeaderBoardExpiry - secondsFromTimeOfDay localTime
      driverLeaderBoardKey = EndRideInt.makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType False merchantOpCityId fromDate toDate
      cachedDriverLeaderBoardKey = EndRideInt.makeDriverLeaderBoardKey leaderBoardConfig.leaderBoardType True merchantOpCityId fromDate toDate
  Hedis.zAddExp driverLeaderBoardKey ride.driverId.getId calculateCurrentZscore leaderBoardExpiry.getSeconds
  let limit = integerFromInt leaderBoardConfig.leaderBoardLengthLimit
  driversListWithScores' <- Hedis.zrevrangeWithscores driverLeaderBoardKey 0 (limit - 1)
  Hedis.setExp cachedDriverLeaderBoardKey driversListWithScores' calculateTotalExpiry.getSeconds
  where
    calculateLeaderBoardExpiry :: Seconds
    calculateLeaderBoardExpiry =
      case leaderBoardConfig.leaderBoardType of
        LConfig.DAILY -> leaderBoardConfig.leaderBoardExpiry
        LConfig.WEEKLY ->
          let (_, currDayIndex) = sundayStartWeek rideDate
           in leaderBoardConfig.leaderBoardExpiry - Seconds ((currDayIndex + 1) * 86400) + Seconds 86400
        LConfig.MONTHLY -> Seconds $ integerToInt $ diffDays toDate rideDate * 86400 + 86400

    calculateCurrentZscore :: Integer
    calculateCurrentZscore =
      fromIntegral $ case driverZscore of
        Nothing -> leaderBoardConfig.zScoreBase + getMeters (fromMaybe 0 rideChargeableDistance)
        Just zscore ->
          let (prevTotalRides, prevTotalDistance) = EndRideInt.getRidesAndDistancefromZscore zscore leaderBoardConfig.zScoreBase
              currTotalRides = prevTotalRides + 1
              currTotalDist = prevTotalDistance + fromMaybe 0 rideChargeableDistance
           in currTotalRides * leaderBoardConfig.zScoreBase + getMeters currTotalDist

    calculateTotalExpiry :: Seconds
    calculateTotalExpiry =
      case leaderBoardConfig.leaderBoardType of
        LConfig.MONTHLY -> Seconds $ integerToInt $ diffDays (getEndDateMonth rideDate leaderBoardConfig.numberOfSets) fromDate * 86400
        _ -> Seconds $ leaderBoardConfig.leaderBoardExpiry.getSeconds * leaderBoardConfig.numberOfSets

getStartDateMonth :: Day -> Day
getStartDateMonth day = fromGregorian y m 1
  where
    (y, m, _) = toGregorian day

getEndDateMonth :: Day -> Int -> Day
getEndDateMonth day addMonths = pred $ addGregorianMonthsClip (integerFromInt addMonths) $ getStartDateMonth day
