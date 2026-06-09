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
    getOTPValidRideCountByDriverIdKey,
    incrementMetroRideCount,
    EventFlow,
    runCancellationLogic,
    updateEventAndGetCoinsvalue,
    getValidRideCountForReward,
  )
where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (utctDay)
import qualified Domain.Types.Coins.CoinHistory as DTCC
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
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.App (lookupCloudType)
import Kernel.Utils.Common
import qualified Lib.DriverCoins.CoinBalance as CoinBalance
import qualified Lib.DriverCoins.CoinLedger as CoinLedger
import qualified Lib.DriverCoins.CoinNotifications as CoinNotifications
import Lib.DriverCoins.Types
import qualified Lib.DriverCoins.Types as DCT
import Lib.Scheduler.Environment (JobCreator)
import Lib.Scheduler.JobStorageType.SchedulerType ()
import Lib.Yudhishthira.Storage.Beam.BeamFlow (HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.CancellationCoins as CancellationCoins
import SharedLogic.External.LocationTrackingService.Types (HasLocationService)
import qualified SharedLogic.Rewards.DriverRewardDispatch as DriverRewardDispatch
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.CoinsConfig as CDCQ
import qualified Storage.CachedQueries.MonetaryRewardConfig as CWCQ
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FleetConfig as QFC
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDAE
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Tools.DynamicLogic as TDL
import Tools.Metrics (CoreMetrics)
import Tools.Utils
import Utils.Common.Cac.KeyNameConstants

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m, ClickhouseFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r)

getCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m Int
getCoinsByDriverId = CoinBalance.getCoinsByDriverId

updateCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Seconds -> m ()
updateCoinsByDriverId driverId coinUpdateValue timeDiffFromUtc = do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let currentDate = show $ utctDay istTime
  expirationPeriod <- CoinBalance.getExpirationSeconds timeDiffFromUtc
  safeIncrBy (CoinBalance.mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral coinUpdateValue) driverId timeDiffFromUtc
  Hedis.withCrossAppRedis $ Hedis.expire (CoinBalance.mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

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
    expirationPeriod <- CoinBalance.getExpirationSeconds timeDiffFromUtc
    safeIncrBy (CoinBalance.mkCoinAccumulationByDriverIdKey driverId currentDate) (negate $ fromIntegral todayAddedCoins) driverId timeDiffFromUtc
    Hedis.withCrossAppRedis $ Hedis.expire (CoinBalance.mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

driverCoinsEvent ::
  ( EventFlow m r,
    ClickhouseFlow m r,
    HasYudhishthiraTablesSchema,
    CoreMetrics m,
    HasLocationService m r,
    JobCreator r m,
    HasShortDurationRetryCfg r c
  ) =>
  Id DP.Person ->
  Maybe DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DCT.DriverCoinsEventType ->
  Maybe Text ->
  Maybe DTVeh.VehicleVariant ->
  Maybe DTC.ServiceTierType ->
  Maybe [LYT.ConfigVersionMap] ->
  m ()
driverCoinsEvent driverId mbDriver merchantId merchantOpCityId eventType entityId mbVehVarient mbServiceTierType mbConfigVersionMap = do
  let vehCategory = maybe (DTVeh.getVehicleCategoryFromVehicleVariantDefault mbVehVarient) DTVeh.castServiceTierToVehicleCategory mbServiceTierType
      tripCatType = case eventType of
        DCT.EndRide {tripCategoryType} -> tripCategoryType
        _ -> DCT.DynamicOfferTrip
  logDebug $ "Driver Coins Event Triggered for merchantOpCityId - " <> merchantOpCityId.getId <> " and driverId - " <> driverId.getId <> "and vehicle category - " <> show vehCategory
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
  rewardHandled <-
    DriverRewardDispatch.tryDispatchDriverRewards driverId mbDriver merchantId merchantOpCityId eventType entityId mbVehVarient mbServiceTierType mbConfigVersionMap
  unless rewardHandled $
    if fromMaybe False transporterConfig.enableDirectWalletIncentives
      then driverMonetaryRewardEvent driverId merchantId merchantOpCityId eventType entityId vehCategory mbConfigVersionMap transporterConfig combinedBlacklist mbFleetOwnerId
      else do
        coinConfiguration <- CDCQ.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehCategory mbServiceTierType tripCatType mbConfigVersionMap
        let applicableCoinConfiguration =
              if null incentiveCohortFunctions
                then coinConfiguration
                else filter (\cc -> cc.eventFunction `elem` incentiveCohortFunctions) coinConfiguration

        let filteredConfigAll =
              if null incentiveCohortFunctions
                then filter (\cc -> cc.eventFunction `notElem` combinedBlacklist && not (isDriverIncentiveCohortFunction cc.eventFunction)) applicableCoinConfiguration
                else filter (\cc -> cc.eventFunction `notElem` combinedBlacklist) applicableCoinConfiguration

        logInfo $ "Coin events for driver " <> driverId.getId <> " - DriverBlacklist: " <> show blacklistedEventsByDriver <> ", FleetBlacklist: " <> show blacklistedEventsByFleet <> ", IncentiveFunctions: " <> show incentiveCohortFunctions <> ", Total: " <> show (map (.eventFunction) coinConfiguration) <> ", Applicable: " <> show (map (.eventFunction) applicableCoinConfiguration) <> ", Filtered: " <> show (map (.eventFunction) filteredConfigAll)

        if null filteredConfigAll
          then do
            logInfo "All coin events blacklisted; skipping award"
            pure ()
          else do
            finalCoinsValue <- sum <$> forM filteredConfigAll (\cc -> calculateCoins eventType driverId merchantId merchantOpCityId cc.eventFunction cc.expirationAt cc.coins transporterConfig entityId vehCategory mbServiceTierType)
            logInfo $ "Awarding coins: " <> show finalCoinsValue
            updateDriverCoins driverId finalCoinsValue transporterConfig.timeDiffFromUtc

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
  _ -> False

calculateCoins :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
calculateCoins eventType driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType = do
  case eventType of
    DCT.Rating {..} -> hRating driverId merchantId merchantOpCityId ratingValue ride eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
    DCT.EndRide {..} -> hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide ride metroRideType tripCategoryType eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory mbServiceTierType
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
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.BonusQuizCoins ->
      runActionWhenValidConditions
        [ pure True
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
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
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.FiveStarRating ->
      runActionWhenValidConditions
        [ pure $ ratingValue == 5,
          pure $ isValidRide ride
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    _ -> pure 0

hEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Maybe Int -> DR.Ride -> MetroRideType -> DCT.TripCategoryType -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide _ride metroRideType tripCategoryType eventFunction mbexpirationTime numCoins _ entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle EndRide Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.RidesCompleted a -> do
      validRideCount <- case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidConditions
        [ pure (validRideCount == a)
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.DriverIncentiveCohortRidesCompleted a -> do
      validRideCount <- case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidConditions
        [ pure (validRideCount == a)
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.PurpleRideCompleted ->
      runActionWhenValidConditions
        [ pure isDisabled
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.GoldTierRideCompleted -> do
      let goldTierRide = isJust coinsRewardedOnGoldTierRide
      runActionWhenValidConditions
        [ pure goldTierRide
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    DCT.MetroRideCompleted mRideType maybeCount -> do
      metroRideCount <- fromMaybe 0 <$> getMetroRideCountByDriverIdKey driverId mRideType
      logDebug $ "Metro Ride Type DB - " <> show mRideType <> "and count - " <> show maybeCount <> "Metro Ride Count from Redis - " <> show metroRideCount
      let conditionsForEveryRide = [pure (mRideType == metroRideType)]
      let conditionsForXRide = maybe [pure False] (\cnt -> conditionsForEveryRide ++ [pure (metroRideCount == cnt)]) maybeCount
      if isJust maybeCount
        then
          runActionWhenValidConditions
            conditionsForXRide
            $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
        else
          if isNothing maybeCount
            then
              runActionWhenValidConditions
                conditionsForEveryRide
                $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
            else pure 0
    _ -> pure 0

hDriverReferral :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DR.Ride -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
hDriverReferral driverId merchantId merchantOpCityId ride eventFunction mbexpirationTime numCoins _ entityId vehCategory mbServiceTierType = do
  logDebug $ "Driver Coins Handle Referral Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.DriverReferral ->
      runActionWhenValidConditions
        [ pure $ isValidRide ride
        ]
        $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType
    _ -> pure 0

validateCancellation :: EventFlow m r => Maybe Text -> UTCTime -> Maybe Meters -> Maybe Meters -> TransporterConfig -> DCT.CancellationType -> m Int
validateCancellation rideId rideStartTime initialDisToPickup cancellationDisToPickup transporterConfig cancelledBy = do
  now <- getCurrentTime

  rideInfo <- case rideId of
    Nothing -> throwError $ RideNotFound "RideId is not present"
    Just rideIdText -> do
      ride <- QRide.findById (Id rideIdText) >>= fromMaybeM (RideNotFound rideIdText)
      let bookingId = ride.bookingId.getId
      booking <- QBooking.findById (Id bookingId) >>= fromMaybeM (BookingNotFound bookingId)
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
      runActionWhenValidConditions [pure False] $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory mbServiceTierType -- To be deprecated
    DCT.BookingCancellationPenalisaton -> do
      numCoinValue <- validateCancellation entityId rideStartTime intialDisToPickup cancellationDisToPickup transporterConfig cancelledBy
      runActionWhenValidConditions [pure (numCoinValue < 0)] $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoinValue entityId vehCategory mbServiceTierType
    DCT.BookingCancellationCompensation -> do
      numCoinValue <- validateCancellation entityId rideStartTime intialDisToPickup cancellationDisToPickup transporterConfig cancelledBy
      runActionWhenValidConditions [pure (numCoinValue > 0)] $ CoinLedger.updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoinValue entityId vehCategory mbServiceTierType
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

incrementValidRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
incrementValidRideCount driverId expirationPeriod incrementValue = do
  validRideCountKeyExists <- getValidRideCountByDriverIdKey driverId
  case validRideCountKeyExists of
    Just _ -> void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral incrementValue)
    Nothing -> setValidRideCountByDriverIdKey driverId expirationPeriod incrementValue

-- | Wallet Incentive Pipeline
driverMonetaryRewardEvent :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsEventType -> Maybe Text -> DTV.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> TransporterConfig -> [DCT.DriverCoinsFunctionType] -> Maybe (Id DP.Person) -> m ()
driverMonetaryRewardEvent driverId merchantId merchantOpCityId eventType entityId vehCategory mbConfigVersionMap transporterConfig combinedBlacklist mbFleetOwnerId = do
  monetaryRewardConfiguration <- CWCQ.fetchMonetaryRewardFunctionsOnEventbasis eventType merchantId merchantOpCityId vehCategory mbConfigVersionMap
  let filteredConfigAll = filter (\wc -> DWC.eventFunction wc `notElem` combinedBlacklist) monetaryRewardConfiguration

  if null filteredConfigAll
    then do
      logInfo "All wallet events blacklisted; skipping award"
      pure ()
    else do
      forM_ filteredConfigAll (\wc -> calculateMonetaryRewardCash eventType driverId merchantId merchantOpCityId (DWC.eventFunction wc) (DWC.monetaryRewardAmount wc) transporterConfig entityId vehCategory mbFleetOwnerId)

calculateMonetaryRewardCash :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> Maybe (Id DP.Person) -> m Bool
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

hMonetaryRewardEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.TripCategoryType -> DCT.DriverCoinsFunctionType -> HighPrecMoney -> TransporterConfig -> Maybe Text -> Maybe (Id DP.Person) -> m Bool
hMonetaryRewardEndRide driverId merchantId merchantOpCityId tripCategoryType eventFunction amount transporterConfig entityId mbFleetOwnerId = do
  case eventFunction of
    DCT.RidesCompleted a -> do
      validRideCount <- case tripCategoryType of
        DCT.OTPRideTrip -> fromMaybe 0 <$> getOTPValidRideCountByDriverIdKey driverId
        DCT.DynamicOfferTrip -> fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidMonetaryRewardConditions [pure (validRideCount == a)] $ CoinLedger.updateEventAndGetMonetaryRewardCredit driverId merchantId merchantOpCityId eventFunction amount transporterConfig entityId mbFleetOwnerId
    _ -> pure False

mkValidRideCountByDriverIdKey :: Id DP.Person -> Text
mkValidRideCountByDriverIdKey driverId = "DriverValidRideCount:DriverId:" <> driverId.getId

getValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Int)
getValidRideCountByDriverIdKey driverId = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkValidRideCountByDriverIdKey driverId)

getValidRideCountForReward :: (MonadFlow m, Hedis.HedisFlow m r) => Id DP.Person -> DCT.DriverCoinsEventType -> m Int
getValidRideCountForReward driverId eventType = case eventType of
  DCT.EndRide {tripCategoryType} -> case tripCategoryType of
    DCT.OTPRideTrip ->
      fromMaybe 0 <$> Hedis.runInMasterCloudRedisCellWithCrossAppRedis (Hedis.get $ mkOTPValidRideCountByDriverIdKey driverId)
    DCT.DynamicOfferTrip ->
      fromMaybe 0 <$> Hedis.runInMasterCloudRedisCellWithCrossAppRedis (Hedis.get $ mkValidRideCountByDriverIdKey driverId)
  _ -> pure 0

setValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
setValidRideCountByDriverIdKey driverId expirationPeriod count = do
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire (mkValidRideCountByDriverIdKey driverId) expirationPeriod

mkOTPValidRideCountByDriverIdKey :: Id DP.Person -> Text
mkOTPValidRideCountByDriverIdKey driverId = "DriverOTPValidRideCount:DriverId:" <> driverId.getId

getOTPValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Int)
getOTPValidRideCountByDriverIdKey driverId = Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.get (mkOTPValidRideCountByDriverIdKey driverId)

setOTPValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
setOTPValidRideCountByDriverIdKey driverId expirationPeriod count = do
  void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkOTPValidRideCountByDriverIdKey driverId) (fromIntegral count)
  Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.expire (mkOTPValidRideCountByDriverIdKey driverId) expirationPeriod

incrementOTPValidRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
incrementOTPValidRideCount driverId expirationPeriod incrementValue = do
  otpValidRideCountKeyExists <- getOTPValidRideCountByDriverIdKey driverId
  case otpValidRideCountKeyExists of
    Just _ -> void $ Hedis.runInMasterCloudRedisCellWithCrossAppRedis $ Hedis.incrby (mkOTPValidRideCountByDriverIdKey driverId) (fromIntegral incrementValue)
    Nothing -> setOTPValidRideCountByDriverIdKey driverId expirationPeriod incrementValue

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

updateEventAndGetCoinsvalue :: (EventFlow m r, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> Maybe Text -> DTV.VehicleCategory -> Maybe DTC.ServiceTierType -> m Int
updateEventAndGetCoinsvalue = CoinLedger.updateEventAndGetCoinsvalue

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey = CoinBalance.mkCoinAccumulationByDriverIdKey

getCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey = CoinBalance.getCoinAccumulationByDriverIdKey

setCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> Int -> Int -> m ()
setCoinAccumulationByDriverIdKey = CoinBalance.setCoinAccumulationByDriverIdKey

getExpirationSeconds :: MonadFlow m => Seconds -> m Int
getExpirationSeconds = CoinBalance.getExpirationSeconds

sendCoinsNotification :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotification = CoinNotifications.sendCoinsNotification

sendCoinsNotificationV2 :: (EventFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) => Id DMOC.MerchantOperatingCity -> Id DP.Person -> HighPrecMoney -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV2 = CoinNotifications.sendCoinsNotificationV2
