{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.Coins
  ( driverCoinsEvent,
    mkCoinAccumulationByDriverIdKey,
    getCoinAccumulationByDriverIdKey,
    setCoinAccumulationByDriverIdKey,
    getCoinsByDriverId,
    getExpirationSeconds,
    incrementValidRideCount,
    incrementOTPValidRideCount,
    updateDriverCoins,
    resetTodayCoinsAndAdjustLifetime,
    sendCoinsNotification,
    sendCoinsNotificationV2,
    safeIncrBy,
    getValidRideCountByDriverIdKey,
    getValidRideCountByDriverIdWindowKey,
    getOTPValidRideCountByDriverIdKey,
    incrementMetroRideCount,
    incrementIncentiveMetricsForRide,
    incrementValidRideCountForTimeBoundCohort,
    EventFlow,
    runCancellationLogic,
    updateEventAndGetCoinsvalue,
  )
where

import qualified Data.Aeson as A
import Data.List (nub)
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Coins.CoinsConfig as DTCoinsConfig
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverStats as DDS
import qualified Domain.Types.FleetConfig as DFC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MonetaryRewardConfig as DWC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Domain.Types.TransporterConfig
import Domain.Types.VehicleCategory as DTV
import qualified Domain.Types.VehicleVariant as DTVeh
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Types as L
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.App (lookupCloudType)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import Lib.DriverCoins.Types
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.CancellationCoins as CancellationCoins
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import qualified SharedLogic.Finance.Wallet as SLFW
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.MonetaryRewardConfig as CWCQ
import Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDAE
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Translations as MTQuery
import qualified Tools.DynamicLogic as TDL
import qualified Tools.Notifications as Notify
import Tools.Utils

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m, ClickhouseFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r)

getCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m Int
getCoinsByDriverId driverId timeDiffFromUtc = Hedis.withLockRedisAndReturnValue driverId.getId 60 $ do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let currentDate = show $ utctDay istTime
  expirationPeriod <- getExpirationSeconds timeDiffFromUtc
  coinKeyExists <- getCoinAccumulationByDriverIdKey driverId currentDate
  case coinKeyExists of
    Just coinBalance -> pure coinBalance
    Nothing -> do
      totalCoins <- CHistory.getTotalCoins driverId (secondsToNominalDiffTime timeDiffFromUtc)
      let coinBalance = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) totalCoins
      Hedis.whenWithLockRedis (mkCoinAccumulationByDriverIdKey driverId currentDate) 60 $ do
        setCoinAccumulationByDriverIdKey driverId currentDate coinBalance expirationPeriod
      pure coinBalance

updateCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Seconds -> m ()
updateCoinsByDriverId driverId coinUpdateValue timeDiffFromUtc = do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let currentDate = show $ utctDay istTime
  expirationPeriod <- getExpirationSeconds timeDiffFromUtc
  safeIncrBy (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral coinUpdateValue) driverId timeDiffFromUtc
  Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

updateDriverCoins :: EventFlow m r => Id DP.Person -> Int -> Seconds -> m ()
updateDriverCoins driverId finalCoinsValue timeDiffFromUtc = do
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  cloudType <- liftIO $ lookupCloudType
  let runQuery = if Just cloudType /= driver.cloudType then Hedis.runInMultiCloudRedisWrite else \x -> x
  void $ Person.updateTotalEarnedCoins (finalCoinsValue + driver.totalEarnedCoins) driverId
  void $ runQuery $ updateCoinsByDriverId driverId finalCoinsValue timeDiffFromUtc

resetTodayCoinsAndAdjustLifetime :: EventFlow m r => Id DP.Person -> Seconds -> m ()
resetTodayCoinsAndAdjustLifetime driverId timeDiffFromUtc = do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
      currentDate = show $ utctDay istTime
  todayCoinHistory <- B.runInReplica $ CHistory.getCoinEventSummary driverId istTime (secondsToNominalDiffTime timeDiffFromUtc)
  let reversalAlreadyApplied = any (\historyItem -> historyItem.eventFunction == DCT.FraudCoinsReversal) todayCoinHistory
      todayAddedCoins = sum $ map (.coins) $ filter (\historyItem -> historyItem.coins > 0) todayCoinHistory
  when (todayAddedCoins > 0 && not reversalAlreadyApplied) $ do
    forM_ (listToMaybe todayCoinHistory) $ \historyItem -> do
      reversalTxnId <- generateGUIDText
      resetEntityId <- generateGUIDText
      CHistory.updateCoinEvent $
        DTCC.CoinHistory
          { id = Id reversalTxnId,
            driverId = driverId.getId,
            merchantId = historyItem.merchantId,
            merchantOptCityId = historyItem.merchantOptCityId,
            eventFunction = DCT.FraudCoinsReversal,
            coins = negate todayAddedCoins,
            status = Used,
            createdAt = now,
            updatedAt = now,
            expirationAt = Nothing,
            coinsUsed = 0,
            bulkUploadTitle = Nothing,
            entityId = Just resetEntityId,
            vehicleCategory = historyItem.vehicleCategory,
            serviceTierType = historyItem.serviceTierType
          }
    driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    void $ Person.updateTotalEarnedCoins (driver.totalEarnedCoins - todayAddedCoins) driverId
    expirationPeriod <- getExpirationSeconds timeDiffFromUtc
    safeIncrBy (mkCoinAccumulationByDriverIdKey driverId currentDate) (negate $ fromIntegral todayAddedCoins) driverId timeDiffFromUtc
    Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

driverCoinsEvent :: (EventFlow m r, Finance.HasActorInfo m r) => Id DP.Person -> Maybe DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsEventType -> Maybe Text -> Maybe DTVeh.VehicleVariant -> Maybe DTC.ServiceTierType -> Maybe [LYT.ConfigVersionMap] -> m ()
driverCoinsEvent driverId mbDriver merchantId merchantOpCityId eventType entityId mbVehVarient mbServiceTierType mbConfigVersionMap = do
  let vehCategory = maybe (DTVeh.getVehicleCategoryFromVehicleVariantDefault mbVehVarient) DTVeh.castServiceTierToVehicleCategory mbServiceTierType
      tripCatType = case eventType of
        DCT.EndRide {tripCategoryType} -> tripCategoryType
        _ -> DCT.DynamicOfferTrip
  logDebug $ "Driver Coins Event Triggered for merchantOpCityId - " <> merchantOpCityId.getId <> " and driverId - " <> driverId.getId <> "and vehicle category - " <> show vehCategory
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mbDriverStats <- B.runInReplica $ QDriverStats.findByPrimaryKey driverId
  logDebug $ "Driver stats present: " <> show (isJust mbDriverStats)
  -- fetch driver fleet here
  mbFleetDriverAssociation <- QFDAE.findByDriverId driverId True
  logDebug $ "Fleet association present: " <> show (isJust mbFleetDriverAssociation)
  -- derive fleetOwnerId and fetch fleet config if present
  let mbFleetOwnerId = (\fda -> Id fda.fleetOwnerId) <$> mbFleetDriverAssociation
  logDebug $ "Fleet owner id: " <> show (getId <$> mbFleetOwnerId)
  mbFleetConfig <- traverse QFC.findByPrimaryKey mbFleetOwnerId
  logDebug $ "Fleet config present: " <> show (maybe False isJust mbFleetConfig)
  -- extract blacklists
  let blacklistedEventsByFleet = case mbFleetConfig of
        Just (Just fc) -> fromMaybe [] (DFC.blacklistCoinEvents fc)
        _ -> []
      blacklistedEventsByDriver = fromMaybe [] (DDS.blacklistCoinEvents =<< mbDriverStats)
      combinedBlacklist = blacklistedEventsByDriver <> blacklistedEventsByFleet
  let incentiveCohortFunctions = maybe [] (extractDriverIncentiveCohortFunctions . (.driverTag)) mbDriver
  if fromMaybe False transporterConfig.enableDirectWalletIncentives
    then driverMonetaryRewardEvent driverId merchantId merchantOpCityId eventType entityId vehCategory mbConfigVersionMap transporterConfig combinedBlacklist mbFleetOwnerId
    else do
      let baseDims =
            CoinsConfigDimensions
              { merchantOptCityId = merchantOpCityId.getId,
                eventName = Just (show eventType),
                merchantId = Just merchantId.getId,
                active = Just True,
                vehicleCategory = Just vehCategory,
                tripCategoryType = Just tripCatType,
                eventFunction = Nothing,
                configId = Nothing,
                serviceTierType = Nothing
              }
      -- Try with serviceTierType first, fall back to Nothing (mirrors old CachedQueries logic)
      coinConfiguration <- do
        result <- case mbServiceTierType of
          Just stt -> do
            res <- getConfig (baseDims {serviceTierType = Just stt}) (Just (SQCC.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehCategory) (Just stt) tripCatType))
            if null res then pure [] else pure res
          Nothing -> pure []
        if null result
          then getConfig baseDims (Just (SQCC.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId (Just vehCategory) Nothing tripCatType))
          else return result
      let applicableCoinConfiguration =
            if null incentiveCohortFunctions
              then coinConfiguration
              else filter (\cc -> cc.eventFunction `elem` incentiveCohortFunctions) coinConfiguration

      let filteredConfigAll =
            if null incentiveCohortFunctions
              then filter (\cc -> cc.eventFunction `notElem` combinedBlacklist && not (isDriverIncentiveCohortFunction cc.eventFunction)) applicableCoinConfiguration
              else filter (\cc -> cc.eventFunction `notElem` combinedBlacklist) applicableCoinConfiguration

      now <- getCurrentTime
      let timeBoundReferenceUtc = case eventType of
            DCT.EndRide {ride} -> rideTimeBoundReferenceUtc ride
            _ -> now
          localTime = addUTCTime (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) timeBoundReferenceUtc
          -- Prefer configs with a real (non-Unbounded) timeBound that matches ride start time.
          -- Fallback: unbounded/null timebound configs (preserves existing day-wide cohort flow).
          letTimeBounds =
            [ CoinConfigWithTimeBounds cc tb
              | cc <- filteredConfigAll,
                Just tb <- [cc.timeBounds],
                tb /= TB.Unbounded
            ]
          wrappedTimeBoundConfigs = TB.findBoundedDomain letTimeBounds localTime
          timeBoundConfigs = (.coinsConfig) <$> wrappedTimeBoundConfigs
          selectedConfigs =
            if null timeBoundConfigs
              then filter (\cc -> fromMaybe TB.Unbounded cc.timeBounds == TB.Unbounded) filteredConfigAll
              else timeBoundConfigs
          metricWindow =
            case listToMaybe wrappedTimeBoundConfigs of
              Just wrapped -> IncentiveMetrics.mkIncentiveWindowKey localTime wrapped.timeBounds
              _ -> IncentiveMetrics.unBoundedWindowKey

      logInfo $ "Coin events for driver " <> driverId.getId <> " - DriverBlacklist: " <> show blacklistedEventsByDriver <> ", FleetBlacklist: " <> show blacklistedEventsByFleet <> ", IncentiveFunctions: " <> show incentiveCohortFunctions <> ", Total: " <> show (map (.eventFunction) coinConfiguration) <> ", Applicable: " <> show (map (.eventFunction) applicableCoinConfiguration) <> ", Filtered: " <> show (map (.eventFunction) filteredConfigAll) <> ", Selected(timeBoundPrefer): " <> show (map (.eventFunction) selectedConfigs)
      logDebug $
        "Coin timebound selection - driverId: "
          <> driverId.getId
          <> ", timeBoundReferenceUtc: "
          <> show timeBoundReferenceUtc
          <> ", localTime: "
          <> show localTime
          <> ", metricWindow: "
          <> show metricWindow
          <> ", candidateTimeBoundConfigs: "
          <> show (map (\c -> (c.coinsConfig.eventFunction, c.timeBounds)) letTimeBounds)
          <> ", matchedTimeBoundConfigs: "
          <> show (map (.eventFunction) timeBoundConfigs)

      if null selectedConfigs
        then do
          logInfo "All coin events blacklisted or no matching timebound/unbounded config; skipping award"
          pure ()
        else do
          finalCoinsValue <-
            sum
              <$> forM
                selectedConfigs
                ( \cc ->
                    calculateCoins
                      eventType
                      driverId
                      merchantId
                      merchantOpCityId
                      cc
                      cc.expirationAt
                      cc.coins
                      transporterConfig
                      entityId
                      vehCategory
                      mbServiceTierType
                      metricWindow
                )
          logInfo $ "Awarding coins: " <> show finalCoinsValue
          updateDriverCoins driverId finalCoinsValue transporterConfig.timeDiffFromUtc

data CoinConfigWithTimeBounds = CoinConfigWithTimeBounds
  { coinsConfig :: DTCoinsConfig.CoinsConfig,
    timeBounds :: TB.TimeBound
  }

extractDriverIncentiveCohortFunctions :: Maybe [LYT.TagNameValueExpiry] -> [DCT.DriverCoinsFunctionType]
extractDriverIncentiveCohortFunctions =
  maybe [] (concatMap parseDriverIncentiveTag)
  where
    parseDriverIncentiveTag (LYT.TagNameValueExpiry rawTagText) =
      case T.splitOn "#" rawTagText of
        ("Incentive" : tagValueText : _) ->
          let parsedFunctions = mapMaybe (readMaybe . T.unpack . T.strip) (T.splitOn "&" tagValueText)
           in filter isDriverIncentiveCohortFunction parsedFunctions
        _ -> []

isDriverIncentiveCohortFunction :: DCT.DriverCoinsFunctionType -> Bool
isDriverIncentiveCohortFunction = \case
  DCT.DriverIncentiveCohortRidesCompleted _ -> True
  DCT.DriverIncentiveCohortMetrics _ -> True
  _ -> False

isDriverIncentiveCohortRidesCompletedFunction :: DCT.DriverCoinsFunctionType -> Bool
isDriverIncentiveCohortRidesCompletedFunction = \case
  DCT.DriverIncentiveCohortRidesCompleted _ -> True
  _ -> False

isDriverIncentiveCohortMetricsFunction :: DCT.DriverCoinsFunctionType -> Bool
isDriverIncentiveCohortMetricsFunction = \case
  DCT.DriverIncentiveCohortMetrics _ -> True
  _ -> False

hasNonUnboundedTimeBound :: DTCoinsConfig.CoinsConfig -> Bool
hasNonUnboundedTimeBound cc = case cc.timeBounds of
  Just tb -> tb /= TB.Unbounded
  Nothing -> False

calculateCoins :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DTCoinsConfig.CoinsConfig -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> IncentiveMetrics.IncentiveWindowKey -> m Int
calculateCoins eventType driverId merchantId merchantOpCityId coinsConfig mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType metricWindow = do
  let eventFunction = coinsConfig.eventFunction
  case eventType of
    DCT.Rating {..} -> hRating driverId merchantId merchantOpCityId ratingValue ride eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    DCT.EndRide {..} -> hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide ride metroRideType tripCategoryType coinsConfig mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType metricWindow
    DCT.DriverToCustomerReferral {..} -> hDriverReferral driverId merchantId merchantOpCityId ride eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    DCT.Cancellation {..} -> hCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup cancelledBy eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    DCT.LMS -> hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    DCT.LMSBonus -> hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    _ -> pure 0

hLms :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins _ entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle LMS Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.QuizQuestionCompleted ->
      runActionWhenValidConditions
        [ pure True
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.BonusQuizCoins ->
      runActionWhenValidConditions
        [ pure True
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    _ -> pure 0

hRating :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Int -> DR.Ride -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hRating driverId merchantId merchantOpCityId ratingValue ride eventFunction mbexpirationTime numCoins _ entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle Rating Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.OneOrTwoStarRating ->
      runActionWhenValidConditions
        [ pure (ratingValue == 1 || ratingValue == 2),
          pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.FiveStarRating ->
      runActionWhenValidConditions
        [ pure $ ratingValue == 5,
          pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    _ -> pure 0

hEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Maybe Int -> DR.Ride -> MetroRideType -> DCT.TripCategoryType -> DTCoinsConfig.CoinsConfig -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> IncentiveMetrics.IncentiveWindowKey -> m Int
hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide _ride metroRideType tripCategoryType coinsConfig mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType metricWindow = do
  let eventFunction = coinsConfig.eventFunction
  logDebug $ "Driver Coins Handle EndRide Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.RidesCompleted a -> do
      validRideCount <- case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidConditions
        [ pure (validRideCount == a)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.DriverIncentiveCohortRidesCompleted a -> do
      -- Timebound config: peak-window ride count. Unbounded: existing day key.
      validRideCount <- getCohortValidRideCount driverId tripCategoryType metricWindow
      logDebug $
        "DriverIncentiveCohortRidesCompleted check - driverId: "
          <> driverId.getId
          <> ", threshold: "
          <> show a
          <> ", window: "
          <> show metricWindow
          <> ", validRideCount: "
          <> show validRideCount
          <> ", configTimeBounds: "
          <> show coinsConfig.timeBounds
      runActionWhenValidConditions
        [ pure (validRideCount == a)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.DriverIncentiveCohortMetrics metrics -> do
      alreadyAwarded <- isIncentiveConfigAlreadyAwarded driverId coinsConfig transporterConfig.timeDiffFromUtc
      metricMatched <- checkAllIncentiveMetricsMet driverId metrics metricWindow
      logDebug $
        "DriverIncentiveCohortMetrics award decision - driverId: "
          <> driverId.getId
          <> ", configId: "
          <> coinsConfig.id.getId
          <> ", window: "
          <> show metricWindow
          <> ", alreadyAwarded: "
          <> show alreadyAwarded
          <> ", metricMatched: "
          <> show metricMatched
      runActionWhenValidConditions
        [ pure (not alreadyAwarded),
          pure metricMatched
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins (Just coinsConfig.id.getId) vehCategory mbServiceTierType
    DCT.PurpleRideCompleted ->
      runActionWhenValidConditions
        [ pure isDisabled
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.GoldTierRideCompleted -> do
      let goldTierRide = isJust coinsRewardedOnGoldTierRide
      runActionWhenValidConditions
        [ pure goldTierRide
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.MetroRideCompleted mRideType maybeCount -> do
      metroRideCount <- fromMaybe 0 <$> getMetroRideCountByDriverIdKey driverId mRideType
      logDebug $ "Metro Ride Type DB - " <> show mRideType <> "and count - " <> show maybeCount <> "Metro Ride Count from Redis - " <> show metroRideCount
      let conditionsForEveryRide = [pure (mRideType == metroRideType)]
      let conditionsForXRide = maybe [pure False] (\cnt -> conditionsForEveryRide ++ [pure (metroRideCount == cnt)]) maybeCount
      if isJust maybeCount
        then
          runActionWhenValidConditions
            conditionsForXRide
            $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
        else
          if isNothing maybeCount
            then
              runActionWhenValidConditions
                conditionsForEveryRide
                $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
            else pure 0
    _ -> pure 0

-- | Award only when every configured (Just) metric threshold is met. Nothing = not required.
checkAllIncentiveMetricsMet :: EventFlow m r => Id DP.Person -> DCT.DriverIncentiveMetrics -> IncentiveMetrics.IncentiveWindowKey -> m Bool
checkAllIncentiveMetricsMet driverId metrics metricWindow = do
  actual <- IncentiveMetrics.getIncentiveMetricsData driverId metricWindow
  let matched =
        IncentiveMetrics.areAllConfiguredMetricsMet
          metrics.ridesCompleted
          actual.ridesCompleted
          metrics.totalEarnings
          actual.totalEarnings
          metrics.totalTripDistanceMeters
          actual.totalTripDistanceMeters
          metrics.totalRideTimeSeconds
          actual.totalRideTimeSeconds
  logDebug $
    "Incentive metrics compare - driverId: "
      <> driverId.getId
      <> ", window: "
      <> show metricWindow
      <> ", thresholds: "
      <> show metrics
      <> ", actual: "
      <> show actual
      <> ", matched: "
      <> show matched
  pure matched

-- | Idempotency: coin_config.id is stored as entityId (same UUID style as ride ids).
-- Scope lookup to the current local day. Safe for TimeBound too when there is at most
-- one peak window per day — morning award will not need a second award later that day.
isIncentiveConfigAlreadyAwarded :: EventFlow m r => Id DP.Person -> DTCoinsConfig.CoinsConfig -> Seconds -> m Bool
isIncentiveConfigAlreadyAwarded driverId coinsConfig timeDiffFromUtc = do
  (windowStart, windowEnd) <- getIncentiveAwardHistoryWindow timeDiffFromUtc
  mbHistory <-
    CHistory.getCoinsByEventFunctionWithinTimeRange
      driverId
      coinsConfig.eventFunction
      (Just coinsConfig.id.getId)
      windowStart
      windowEnd
  pure $ isJust mbHistory

-- | Current local calendar day as UTC [dayStart, dayEnd), using transporter timeDiffFromUtc.
getIncentiveAwardHistoryWindow :: MonadFlow m => Seconds -> m (UTCTime, UTCTime)
getIncentiveAwardHistoryWindow timeDiffFromUtc = do
  now <- getCurrentTime
  let localTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
      localDay = utctDay localTime
      istOffset = negate (secondsToNominalDiffTime timeDiffFromUtc)
      dayStartLocal = UTCTime localDay 0
      dayEndLocal = addUTCTime (24 * 60 * 60) dayStartLocal
  pure (addUTCTime istOffset dayStartLocal, addUTCTime istOffset dayEndLocal)

hDriverReferral :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DR.Ride -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hDriverReferral driverId merchantId merchantOpCityId ride eventFunction mbexpirationTime numCoins _ entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle Referral Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.DriverReferral ->
      runActionWhenValidConditions
        [ pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    _ -> pure 0

validateCancellation :: EventFlow m r => Maybe Text -> UTCTime -> Maybe Meters -> Maybe Meters -> TransporterConfig -> DCT.CancellationType -> m Int
validateCancellation rideId rideStartTime initialDisToPickup cancellationDisToPickup transporterConfig cancelledBy = do
  now <- getCurrentTime

  rideInfo <- case rideId of
    Nothing -> throwError $ RideNotFound "RideId is not present"
    Just rideIdText -> do
      ride <- QRide.findById (Id rideIdText) >>= fromMaybeM (RideNotFound rideIdText)
      let bookingId = ride.bookingId.getId
      booking <- QBookingLite.findByIdLite (Id bookingId) >>= fromMaybeM (BookingNotFound bookingId)
      let quoteId = booking.quoteId
      driverQuote <- QDQ.findById (Id quoteId) >>= fromMaybeM (QuoteNotFound quoteId)
      let estimatedTimeToPickup = secondsToNominalDiffTime driverQuote.durationToPickup
      mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
      let callAttemptByDriver = isJust mbCallStatus
      let isArrivedAtPickup = case cancellationDisToPickup of
            Just disToPickup -> disToPickup < highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
            Nothing -> False
      pure (ride, booking.transactionId, callAttemptByDriver, isArrivedAtPickup, estimatedTimeToPickup)

  let (ride, transactionId, callAttemptByDriver, isArrivedAtPickup, estimatedTimeToPickup) = rideInfo
      timeOfCancellation = round $ diffUTCTime now rideStartTime
      actualCoveredDistance = case (initialDisToPickup, cancellationDisToPickup) of
        (Just initial, Just cancellation) -> Just (initial - cancellation)
        _ -> Nothing
      expectedCoveredDistance =
        if isJust initialDisToPickup
          then
            let initialDistance = fromJust initialDisToPickup
                progressRatio = fromIntegral timeOfCancellation / max 1 estimatedTimeToPickup
                expectedDistance = round $ fromIntegral initialDistance * progressRatio
             in Just expectedDistance
          else Nothing
      driverWaitingTime = if isJust ride.driverArrivalTime then Just (round $ diffUTCTime now (fromJust ride.driverArrivalTime)) else Nothing

  let logicInput =
        CancellationCoins.CancellationCoinData
          { cancelledBy = cancelledBy,
            timeOfDriverCancellation = timeOfCancellation,
            timeOfCustomerCancellation = timeOfCancellation,
            isArrivedAtPickup = isArrivedAtPickup,
            driverWaitingTime = driverWaitingTime,
            callAttemptByDriver = callAttemptByDriver,
            actualCoveredDistance = actualCoveredDistance,
            expectedCoveredDistance = expectedCoveredDistance
          }

  runCancellationLogic ride.merchantOperatingCityId (Just transactionId) logicInput

runCancellationLogic :: EventFlow m r => Id DMOC.MerchantOperatingCity -> Maybe Text -> CancellationCoins.CancellationCoinData -> m Int
runCancellationLogic merchantOpCityId mbEntityTransactionId logicInput = do
  now <- getCurrentTime
  (logics, _) <- TDL.getAppDynamicLogic (cast merchantOpCityId) LYT.CANCELLATION_COIN_POLICY now Nothing Nothing

  if null logics
    then do
      logInfo "No cancellation logic found, using default logic"
      pure 0
    else do
      logInfo $ "Running cancellation logic with " <> show (length logics) <> " rules"
      result <- LYDL.runLogicsWithDebugLog LYDL.Driver (cast merchantOpCityId) LYT.CANCELLATION_COIN_POLICY mbEntityTransactionId logics logicInput
      case A.fromJSON result.result :: A.Result CancellationCoins.CancellationCoinResult of
        A.Success logicResult -> do
          logInfo $ "Cancellation logic result: " <> show logicResult
          pure logicResult.coins
        A.Error err -> do
          logError $ "Failed to parse cancellation logic result: " <> show err
          pure 0

hCancellation :: (EventFlow m r, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Maybe Meters -> Maybe Meters -> DCT.CancellationType -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup cancelledBy eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle Cancellation Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.BookingCancellation -> do
      runActionWhenValidConditions [pure False] $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType -- To be deprecated
    DCT.BookingCancellationPenalisaton -> do
      numCoinValue <- validateCancellation entityId rideStartTime intialDisToPickup cancellationDisToPickup transporterConfig cancelledBy
      runActionWhenValidConditions [pure (numCoinValue < 0)] $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoinValue entityId vehCategory mbServiceTierType
    DCT.BookingCancellationCompensation -> do
      numCoinValue <- validateCancellation entityId rideStartTime intialDisToPickup cancellationDisToPickup transporterConfig cancelledBy
      runActionWhenValidConditions [pure (numCoinValue > 0)] $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoinValue entityId vehCategory mbServiceTierType
    _ -> pure 0

runActionWhenValidConditions :: EventFlow m r => [m Bool] -> m Int -> m Int
runActionWhenValidConditions conditions action = do
  isValid <- checkAllConditions conditions
  if isValid
    then do action
    else pure 0
  where
    checkAllConditions [] = pure True
    checkAllConditions (condition : xs) = do
      isValid <- condition
      if isValid then checkAllConditions xs else pure False

updateEventAndGetCoinsvalue :: (EventFlow m r, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currentBalance <- getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  let expiryTime =
        if currentBalance < 0
          then Nothing -- if current balance is negative we dont expire the coins.
          else fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
      status_ = if numCoins > 0 then Remaining else Used
  let driverCoinEvent =
        DTCC.CoinHistory
          { id = Id uuid,
            driverId = driverId.getId,
            merchantId = merchantId.getId,
            merchantOptCityId = merchantOpCityId.getId,
            eventFunction = eventFunction,
            coins = numCoins,
            status = status_,
            createdAt = now,
            updatedAt = now,
            expirationAt = expiryTime,
            coinsUsed = 0,
            bulkUploadTitle = Nothing,
            entityId = entityId,
            vehicleCategory = Just vehCategory,
            serviceTierType = mbServiceTierType
          }
  CHistory.updateCoinEvent driverCoinEvent

  case eventFunction of
    DCT.BookingCancellationPenalisaton -> do
      sendCoinsNotification merchantOpCityId driverId numCoins eventFunction
    _ -> do
      when (numCoins > 0) $ do
        case eventFunction of
          DCT.MetroRideCompleted _ _ -> do
            -- case match to be removed after next deployment
            logDebug "metro notification case for coins"
            sendCoinsNotificationV3 merchantOpCityId driverId numCoins eventFunction
          _ -> sendCoinsNotification merchantOpCityId driverId numCoins eventFunction
  pure numCoins

-- This function is to be removed after next apk deployment
sendCoinsNotificationV3 :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV3 merchantOpCityId driverId coinsValue (DCT.MetroRideCompleted metroRideType _) =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = DCT.MetroRideCompleted metroRideType Nothing}
     in getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOpCityId.getId, messageKey = T.pack (show metroRideType), language = Just language}) (Just (MTQuery.findByErrorAndLanguage (T.pack (show metroRideType)) language)) >>= processMessage driver entityData
  where
    processMessage driver entityData mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] ->
              Notify.sendCoinsNotificationV3 merchantOpCityId title (replaceCoinsValue description) driver (driver.deviceToken) entityData metroRideType
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#pointsValue#}" (T.pack $ show coinsValue)
sendCoinsNotificationV3 _merchantOpCityId _driverId _coinsValue _eventFunction = pure ()

sendCoinsNotificationV2 :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> HighPrecMoney -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV2 merchantOpCityId driverId amount coinsValue (DCT.BulkUploadFunctionV2 messageKey) = do
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
     in getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOpCityId.getId, messageKey = T.pack (show messageKey), language = Just language}) (Just (MTQuery.findByErrorAndLanguage (T.pack (show messageKey)) language)) >>= processMessage driver amount
  where
    processMessage driver amount' mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] -> do
              let descriptionReplaced = replaceAmountValue amount' $ replaceCoinsValue description
              Notify.sendNotificationToDriver merchantOpCityId FCM.SHOW Nothing FCM.COINS_SUCCESS title descriptionReplaced driver (driver.deviceToken)
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#coinsValue#}" (T.pack $ show coinsValue)
    replaceAmountValue amount' = T.replace "{#amountValue#}" (T.pack $ show amount')
sendCoinsNotificationV2 _merchantOpCityId _driverId _amount _coinsValue _eventFunction = pure ()

sendCoinsNotification :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotification merchantOpCityId driverId coinsValue eventFunction =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        queryType = if coinsValue > 0 then CoinAdded else CoinSubtracted
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = eventFunction}
     in getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOpCityId.getId, messageKey = T.pack (show queryType), language = Just language}) (Just (MTQuery.findByErrorAndLanguage (T.pack (show queryType)) language)) >>= processMessage driver entityData
  where
    processMessage driver entityData mbCoinsMessage =
      case mbCoinsMessage of
        Just coinsMessage ->
          case T.splitOn " | " coinsMessage.message of
            [title, description] ->
              Notify.sendCoinsNotification merchantOpCityId title (replaceCoinsValue description) driver (driver.deviceToken) entityData
            _ -> logDebug "Invalid message format."
        Nothing -> logDebug "Could not find Translations."
    replaceCoinsValue = T.replace "{#coinsValue#}" (T.pack $ show coinsValue)

getExpirationSeconds :: MonadFlow m => Seconds -> m Int
getExpirationSeconds timeDiffFromUtc = do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let expirationSeconds = round $ diffUTCTime (UTCTime (addDays 1 $ utctDay istTime) 0) istTime -- expire at 12:00 AM IST
  pure expirationSeconds

incrementValidRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
incrementValidRideCount driverId expirationPeriod incrementValue = do
  validRideCountKeyExists <- getValidRideCountByDriverIdKey driverId
  case validRideCountKeyExists of
    Just _ -> void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral incrementValue)
    Nothing -> setValidRideCountByDriverIdKey driverId expirationPeriod incrementValue

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey driverId date = "DriverCoinBalance:DriverId:" <> driverId.getId <> ":" <> date

-- | Wallet Incentive Pipeline
driverMonetaryRewardEvent :: (EventFlow m r, Finance.HasActorInfo m r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsEventType -> Maybe Text -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> TransporterConfig -> [DCT.DriverCoinsFunctionType] -> Maybe (Id DP.Person) -> m ()
driverMonetaryRewardEvent driverId merchantId merchantOpCityId eventType entityId vehCategory mbConfigVersionMap transporterConfig combinedBlacklist mbFleetOwnerId = do
  monetaryRewardConfiguration <- CWCQ.fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId vehCategory mbConfigVersionMap
  let filteredConfigAll = filter (\wc -> DWC.eventFunction wc `notElem` combinedBlacklist) monetaryRewardConfiguration

  if null filteredConfigAll
    then do
      logInfo "All wallet events blacklisted; skipping award"
      pure ()
    else do
      forM_ filteredConfigAll (\wc -> calculateMonetaryRewardCash eventType driverId merchantId merchantOpCityId (DWC.eventFunction wc) (DWC.monetaryRewardAmount wc) transporterConfig entityId vehCategory mbFleetOwnerId)

calculateMonetaryRewardCash :: (EventFlow m r, Finance.HasActorInfo m r) => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe (Id DP.Person) -> m Bool
calculateMonetaryRewardCash eventType driverId merchantId merchantOpCityId eventFunction amount transporterConfig entityId _ mbFleetOwnerId = do
  case eventType of
    DCT.EndRide {tripCategoryType} -> hMonetaryRewardEndRide driverId merchantId merchantOpCityId tripCategoryType eventFunction amount transporterConfig entityId mbFleetOwnerId
    _ -> pure False

runActionWhenValidMonetaryRewardConditions :: EventFlow m r => [m Bool] -> m Bool -> m Bool
runActionWhenValidMonetaryRewardConditions conditions action = do
  isValid <- checkAllConditions conditions
  if isValid
    then do action
    else pure False
  where
    checkAllConditions [] = pure True
    checkAllConditions (condition : xs) = do
      isValid <- condition
      if isValid then checkAllConditions xs else pure False

hMonetaryRewardEndRide :: (EventFlow m r, Finance.HasActorInfo m r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.TripCategoryType -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> Maybe (Id DP.Person) -> m Bool
hMonetaryRewardEndRide driverId merchantId merchantOpCityId tripCategoryType eventFunction amount transporterConfig entityId mbFleetOwnerId = do
  case eventFunction of
    DCT.RidesCompleted a -> do
      validRideCount <- case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidMonetaryRewardConditions [pure (validRideCount == a)] $ updateEventAndGetMonetaryRewardCredit driverId merchantId merchantOpCityId eventFunction amount transporterConfig entityId mbFleetOwnerId
    _ -> pure False

updateEventAndGetMonetaryRewardCredit :: (EventFlow m r, Finance.HasActorInfo m r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> Maybe (Id DP.Person) -> m Bool
updateEventAndGetMonetaryRewardCredit driverId merchantId merchantOpCityId eventFunction amount transporterConfig entityId mbFleetOwnerId = do
  let referenceId = fromMaybe "wallet_incentive" entityId
  logDebug $
    "Wallet Incentive: crediting driver wallet"
      <> " | driverId="
      <> driverId.getId
      <> " | eventFunction="
      <> show eventFunction
      <> " | amount="
      <> show amount
      <> " | currency="
      <> show transporterConfig.currency
      <> " | referenceId="
      <> referenceId
  let (counterparty, ownerId) = case mbFleetOwnerId of
        Just fleetOwnerId -> (counterpartyFleetOwner, fleetOwnerId.getId)
        Nothing -> (counterpartyDriver, driverId.getId)
  res <-
    SLFW.createWalletEntryDelta
      counterparty
      ownerId
      amount
      transporterConfig.currency
      merchantId.getId
      merchantOpCityId.getId
      SLFW.walletReferenceWalletIncentive
      referenceId
      Nothing
  case res of
    Right _ -> pure True
    Left err -> do
      logError $ "Failed to credit driver wallet: " <> show err
      pure False

getCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey driverId currentDate = Hedis.withCrossAppRedis $ Hedis.get (mkCoinAccumulationByDriverIdKey driverId currentDate)

setCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> Int -> Int -> m ()
setCoinAccumulationByDriverIdKey driverId currentDate count expirationPeriod = do
  void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral count)
  Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

mkValidRideCountByDriverIdKey :: Id DP.Person -> Text
mkValidRideCountByDriverIdKey driverId = "DriverValidRideCount:DriverId:" <> driverId.getId

-- | Timebound-scoped valid ride count (DriverIncentiveCohortRidesCompleted peak window).
-- Day/unbounded path keeps using mkValidRideCountByDriverIdKey (no :Window: suffix).
mkValidRideCountByDriverIdWindowKey :: Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Text
mkValidRideCountByDriverIdWindowKey driverId windowKey =
  mkValidRideCountByDriverIdKey driverId <> ":Window:" <> IncentiveMetrics.windowSuffix windowKey

getValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Int)
getValidRideCountByDriverIdKey driverId = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkValidRideCountByDriverIdKey driverId)

getValidRideCountByDriverIdWindowKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> m (Maybe Int)
getValidRideCountByDriverIdWindowKey driverId windowKey =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkValidRideCountByDriverIdWindowKey driverId windowKey)

setValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
setValidRideCountByDriverIdKey driverId expirationPeriod count = do
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire (mkValidRideCountByDriverIdKey driverId) expirationPeriod

setValidRideCountByDriverIdWindowKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Int -> Int -> m ()
setValidRideCountByDriverIdWindowKey driverId windowKey expirationPeriod count = do
  let key = mkValidRideCountByDriverIdWindowKey driverId windowKey
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby key (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire key expirationPeriod

incrementValidRideCountInWindow :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Int -> Int -> m ()
incrementValidRideCountInWindow driverId windowKey expirationPeriod incrementValue = do
  mbCount <- getValidRideCountByDriverIdWindowKey driverId windowKey
  case mbCount of
    Just _ ->
      void $
        Hedis.runInMasterCloudRedisCellWithCrossAppRedis $
          Hedis.incrby (mkValidRideCountByDriverIdWindowKey driverId windowKey) (fromIntegral incrementValue)
    Nothing -> setValidRideCountByDriverIdWindowKey driverId windowKey expirationPeriod incrementValue

mkOTPValidRideCountByDriverIdKey :: Id DP.Person -> Text
mkOTPValidRideCountByDriverIdKey driverId = "DriverOTPValidRideCount:DriverId:" <> driverId.getId

mkOTPValidRideCountByDriverIdWindowKey :: Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Text
mkOTPValidRideCountByDriverIdWindowKey driverId windowKey =
  mkOTPValidRideCountByDriverIdKey driverId <> ":Window:" <> IncentiveMetrics.windowSuffix windowKey

getOTPValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Int)
getOTPValidRideCountByDriverIdKey driverId = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkOTPValidRideCountByDriverIdKey driverId)

getOTPValidRideCountByDriverIdWindowKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> m (Maybe Int)
getOTPValidRideCountByDriverIdWindowKey driverId windowKey =
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkOTPValidRideCountByDriverIdWindowKey driverId windowKey)

setOTPValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
setOTPValidRideCountByDriverIdKey driverId expirationPeriod count = do
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkOTPValidRideCountByDriverIdKey driverId) (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire (mkOTPValidRideCountByDriverIdKey driverId) expirationPeriod

setOTPValidRideCountByDriverIdWindowKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Int -> Int -> m ()
setOTPValidRideCountByDriverIdWindowKey driverId windowKey expirationPeriod count = do
  let key = mkOTPValidRideCountByDriverIdWindowKey driverId windowKey
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby key (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire key expirationPeriod

incrementOTPValidRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
incrementOTPValidRideCount driverId expirationPeriod incrementValue = do
  otpValidRideCountKeyExists <- getOTPValidRideCountByDriverIdKey driverId
  case otpValidRideCountKeyExists of
    Just _ -> void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkOTPValidRideCountByDriverIdKey driverId) (fromIntegral incrementValue)
    Nothing -> setOTPValidRideCountByDriverIdKey driverId expirationPeriod incrementValue

incrementOTPValidRideCountInWindow :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> IncentiveMetrics.IncentiveWindowKey -> Int -> Int -> m ()
incrementOTPValidRideCountInWindow driverId windowKey expirationPeriod incrementValue = do
  mbCount <- getOTPValidRideCountByDriverIdWindowKey driverId windowKey
  case mbCount of
    Just _ ->
      void $
        Hedis.runInMasterCloudRedisCellWithCrossAppRedis $
          Hedis.incrby (mkOTPValidRideCountByDriverIdWindowKey driverId windowKey) (fromIntegral incrementValue)
    Nothing -> setOTPValidRideCountByDriverIdWindowKey driverId windowKey expirationPeriod incrementValue

-- | Ride count used by DriverIncentiveCohortRidesCompleted.
-- TimeBoundWindow -> peak-scoped key; DayWindow (unbounded fallback) -> existing day key.
getCohortValidRideCount ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  DCT.TripCategoryType ->
  IncentiveMetrics.IncentiveWindowKey ->
  m Int
getCohortValidRideCount driverId tripCategoryType metricWindow =
  case metricWindow of
    IncentiveMetrics.DayWindow ->
      case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
    IncentiveMetrics.TimeBoundWindow _ ->
      case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdWindowKey driverId metricWindow
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdWindowKey driverId metricWindow

safeIncrBy :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Integer -> Id DP.Person -> Seconds -> m ()
safeIncrBy key value driverId timeDiffFromUtc = do
  _ <- getCoinsByDriverId driverId timeDiffFromUtc
  void $ Hedis.withCrossAppRedis $ Hedis.incrby key value

mkMetroRideCountByDriverIdKey :: Id DP.Person -> DCT.MetroRideType -> Text
mkMetroRideCountByDriverIdKey driverId metroRideType = "DriverMetroRideCount:DriverId:" <> driverId.getId <> ":MetroRideType:" <> show metroRideType

getMetroRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> m (Maybe Int)
getMetroRideCountByDriverIdKey driverId metroRideType = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkMetroRideCountByDriverIdKey driverId metroRideType)

setMetroRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> Int -> Int -> m ()
setMetroRideCountByDriverIdKey driverId metroRideType expirationPeriod count = do
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkMetroRideCountByDriverIdKey driverId metroRideType) (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire (mkMetroRideCountByDriverIdKey driverId metroRideType) expirationPeriod

incrementMetroRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> Int -> Int -> m ()
incrementMetroRideCount driverId metroRideType expirationPeriod incrementValue = do
  metroRideCountKeyExists <- getMetroRideCountByDriverIdKey driverId metroRideType
  case metroRideCountKeyExists of
    Just _ -> void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkMetroRideCountByDriverIdKey driverId metroRideType) (fromIntegral incrementValue)
    Nothing -> setMetroRideCountByDriverIdKey driverId metroRideType expirationPeriod incrementValue

-- | UTC instant used to decide whether a ride falls inside a coin-config timeBound.
rideTimeBoundReferenceUtc :: DR.Ride -> UTCTime
rideTimeBoundReferenceUtc ride = fromMaybe ride.createdAt ride.tripStartTime

-- | When active DriverIncentiveCohortRidesCompleted configs have real timebounds and
-- the ride start falls inside a peak, increment peak-scoped valid-ride-count keys.
-- Day-wide DriverValidRideCount is still updated by incrementValidRideCount (unchanged).
incrementValidRideCountForTimeBoundCohort ::
  EventFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  DCT.TripCategoryType ->
  Int ->
  Seconds ->
  UTCTime ->
  m ()
incrementValidRideCountForTimeBoundCohort driverId merchantId merchantOpCityId vehCategory tripCategoryType expirationPeriod timeDiffFromUtc timeBoundReferenceUtc = do
  configs <- SQCC.getActiveCoinConfigs merchantId merchantOpCityId vehCategory
  let cohortTimeBoundConfigs =
        filter
          ( \cc ->
              cc.eventName == "EndRide"
                && isDriverIncentiveCohortRidesCompletedFunction cc.eventFunction
                && hasNonUnboundedTimeBound cc
          )
          configs
  unless (null cohortTimeBoundConfigs) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) timeBoundReferenceUtc
        incentiveTimeBounds =
          nub $
            mapMaybe
              ( \cc -> case cc.timeBounds of
                  Just tb | tb /= TB.Unbounded -> Just tb
                  _ -> Nothing
              )
              cohortTimeBoundConfigs
        windows = IncentiveMetrics.matchingTimeBoundWindows localTime incentiveTimeBounds
    logDebug $
      "Timebound cohort ride-count increment - driverId: "
        <> driverId.getId
        <> ", tripCategoryType: "
        <> show tripCategoryType
        <> ", timeBoundReferenceUtc: "
        <> show timeBoundReferenceUtc
        <> ", localTime: "
        <> show localTime
        <> ", cohortTimeBoundEventFunctions: "
        <> show (map (.eventFunction) cohortTimeBoundConfigs)
        <> ", matchingWindows: "
        <> show windows
    forM_ windows $ \windowKey ->
      case tripCategoryType of
        DCT.DynamicOfferTrip -> incrementValidRideCountInWindow driverId windowKey expirationPeriod 1
        DCT.OTPRideTrip -> incrementOTPValidRideCountInWindow driverId windowKey expirationPeriod 1

-- | Increment day + matching time-bound incentive metric keys using active coin configs' timeBounds.
incrementIncentiveMetricsForRide ::
  EventFlow m r =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  IncentiveMetrics.RideIncentiveDeltas ->
  Int ->
  Seconds ->
  UTCTime ->
  m ()
incrementIncentiveMetricsForRide driverId merchantId merchantOpCityId vehCategory deltas expirationPeriod timeDiffFromUtc timeBoundReferenceUtc = do
  configs <- SQCC.getActiveCoinConfigs merchantId merchantOpCityId vehCategory
  let metricsConfigs =
        filter
          ( \cc ->
              cc.eventName == "EndRide"
                && isDriverIncentiveCohortMetricsFunction cc.eventFunction
          )
          configs
  unless (null metricsConfigs) $ do
    let localTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) timeBoundReferenceUtc
        incentiveTimeBounds =
          nub $
            map (fromMaybe TB.Unbounded . (.timeBounds)) $
              filter
                ( \cc -> fromMaybe TB.Unbounded cc.timeBounds /= TB.Unbounded
                )
                metricsConfigs
    logDebug $
      "Incentive metrics increment for ride - driverId: "
        <> driverId.getId
        <> ", deltas: "
        <> show deltas
        <> ", timeBoundReferenceUtc: "
        <> show timeBoundReferenceUtc
        <> ", localTime: "
        <> show localTime
        <> ", metricsConfigCount: "
        <> show (length metricsConfigs)
        <> ", timeBoundsCount: "
        <> show (length incentiveTimeBounds)
    IncentiveMetrics.incrementIncentiveMetrics driverId deltas expirationPeriod incentiveTimeBounds localTime
