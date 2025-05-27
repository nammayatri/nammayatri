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
    updateDriverCoins,
    sendCoinsNotification,
    sendCoinsNotificationV2,
    safeIncrBy,
    getValidRideCountByDriverIdKey,
    incrementMetroRideCount,
    EventFlow,
  )
where

import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Domain.Types.TransporterConfig
import Domain.Types.VehicleCategory as DTV
import qualified Domain.Types.VehicleVariant as DTVeh
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Types as L
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.DriverCoins.Types
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.CoinsConfig as CDCQ
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Translations as MTQuery
import qualified Tools.Notifications as Notify
import Tools.Utils
import Utils.Common.Cac.KeyNameConstants

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m)

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
  void $ Person.updateTotalEarnedCoins (finalCoinsValue + driver.totalEarnedCoins) driverId
  updateCoinsByDriverId driverId finalCoinsValue timeDiffFromUtc

driverCoinsEvent :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsEventType -> Maybe Text -> Maybe DTVeh.VehicleVariant -> Maybe [LYT.ConfigVersionMap] -> m ()
driverCoinsEvent driverId merchantId merchantOpCityId eventType entityId mbVehVarient mbConfigVersionMap = do
  let vehCategory = DTVeh.getVehicleCategoryFromVehicleVariantDefault mbVehVarient
  logDebug $ "Driver Coins Event Triggered for merchantOpCityId - " <> merchantOpCityId.getId <> " and driverId - " <> driverId.getId <> "and vehicle category - " <> show vehCategory
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  coinConfiguration <- CDCQ.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId vehCategory mbConfigVersionMap
  finalCoinsValue <- sum <$> forM coinConfiguration (\cc -> calculateCoins eventType driverId merchantId merchantOpCityId cc.eventFunction cc.expirationAt cc.coins transporterConfig entityId vehCategory)
  updateDriverCoins driverId finalCoinsValue transporterConfig.timeDiffFromUtc

calculateCoins :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
calculateCoins eventType driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory = do
  case eventType of
    DCT.Rating {..} -> hRating driverId merchantId merchantOpCityId ratingValue ride eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    DCT.EndRide {..} -> hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide ride metroRideType eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    DCT.DriverToCustomerReferral {..} -> hDriverReferral driverId merchantId merchantOpCityId ride eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    DCT.Cancellation {..} -> hCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    DCT.LMS -> hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    DCT.LMSBonus -> hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory
    _ -> pure 0

hLms :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
hLms driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins _ entityId vehCategory = do
  logDebug $ "Driver Coins Handle LMS Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.QuizQuestionCompleted ->
      runActionWhenValidConditions
        [ pure True
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    DCT.BonusQuizCoins ->
      runActionWhenValidConditions
        [ pure True
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    _ -> pure 0

hRating :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Int -> DR.Ride -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
hRating driverId merchantId merchantOpCityId ratingValue ride eventFunction mbexpirationTime numCoins _ entityId vehCategory = do
  logDebug $ "Driver Coins Handle Rating Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.OneOrTwoStarRating ->
      runActionWhenValidConditions
        [ pure (ratingValue == 1 || ratingValue == 2),
          pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    DCT.FiveStarRating ->
      runActionWhenValidConditions
        [ pure $ ratingValue == 5,
          pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    _ -> pure 0

hEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Maybe Int -> DR.Ride -> MetroRideType -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
hEndRide driverId merchantId merchantOpCityId isDisabled coinsRewardedOnGoldTierRide _ride metroRideType eventFunction mbexpirationTime numCoins _ entityId vehCategory = do
  logDebug $ "Driver Coins Handle EndRide Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.RidesCompleted a -> do
      validRideCount <- fromMaybe 0 <$> getValidRideCountByDriverIdKey driverId
      runActionWhenValidConditions
        [ pure (validRideCount == a)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    DCT.PurpleRideCompleted ->
      runActionWhenValidConditions
        [ pure isDisabled
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    DCT.GoldTierRideCompleted -> do
      let goldTierRide = isJust coinsRewardedOnGoldTierRide
      runActionWhenValidConditions
        [ pure goldTierRide
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    DCT.MetroRideCompleted mRideType maybeCount -> do
      metroRideCount <- fromMaybe 0 <$> getMetroRideCountByDriverIdKey driverId mRideType
      logDebug $ "Metro Ride Type DB - " <> show mRideType <> "and count - " <> show maybeCount <> "Metro Ride Count from Redis - " <> show metroRideCount
      let conditions = [pure (mRideType == metroRideType)] ++ maybe [] (\cnt -> [pure (metroRideCount == cnt)]) maybeCount
      runActionWhenValidConditions
        conditions
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    _ -> pure 0

hDriverReferral :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DR.Ride -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
hDriverReferral driverId merchantId merchantOpCityId ride eventFunction mbexpirationTime numCoins _ entityId vehCategory = do
  logDebug $ "Driver Coins Handle Referral Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.DriverReferral ->
      runActionWhenValidConditions
        [ pure $ isValidRide ride
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
    _ -> pure 0

hCancellation :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Maybe Meters -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> TransporterConfig -> Maybe Text -> DTV.VehicleCategory -> m Int
hCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup eventFunction mbexpirationTime numCoins transporterConfig entityId vehCategory = do
  logDebug $ "Driver Coins Handle Cancellation Event Triggered - " <> show eventFunction
  now <- getCurrentTime
  let timeDiff = diffUTCTime now rideStartTime
  case eventFunction of
    DCT.BookingCancellation -> do
      let validConditions = case (intialDisToPickup, cancellationDisToPickup) of
            (Just intialDis, Just cancellationDis) ->
              timeDiff > transporterConfig.cancellationTimeDiff && intialDis - cancellationDis < fromIntegral transporterConfig.cancellationDistDiff
            _ -> False
      runActionWhenValidConditions [pure validConditions] $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory
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

updateEventAndGetCoinsvalue :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> Maybe Text -> DTV.VehicleCategory -> m Int
updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins entityId vehCategory = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  let expiryTime = fmap (\expirationTime -> UTCTime (utctDay $ addUTCTime (fromIntegral expirationTime) now) 0) mbexpirationTime
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
            vehicleCategory = Just vehCategory
          }
  CHistory.updateCoinEvent driverCoinEvent
  when (numCoins > 0) $ do
    case eventFunction of
      DCT.MetroRideCompleted _ _ -> do
        -- case match to be removed after next deployment
        logDebug "metro notification case for coins"
        sendCoinsNotificationV3 merchantOpCityId driverId numCoins eventFunction
      _ -> sendCoinsNotification merchantOpCityId driverId numCoins eventFunction
  pure numCoins

-- This function is to be removed after next apk deployment
sendCoinsNotificationV3 :: EventFlow m r => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV3 merchantOpCityId driverId coinsValue (DCT.MetroRideCompleted metroRideType _) =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = (DCT.MetroRideCompleted metroRideType Nothing)}
     in MTQuery.findByErrorAndLanguage (T.pack (show metroRideType)) language >>= processMessage driver entityData
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

sendCoinsNotificationV2 :: EventFlow m r => Id DMOC.MerchantOperatingCity -> Id DP.Person -> HighPrecMoney -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotificationV2 merchantOpCityId driverId amount coinsValue (DCT.BulkUploadFunctionV2 messageKey) = do
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
     in MTQuery.findByErrorAndLanguage (T.pack (show messageKey)) language >>= processMessage driver amount
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

sendCoinsNotification :: EventFlow m r => Id DMOC.MerchantOperatingCity -> Id DP.Person -> Int -> DCT.DriverCoinsFunctionType -> m ()
sendCoinsNotification merchantOpCityId driverId coinsValue eventFunction =
  B.runInReplica (Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)) >>= \driver ->
    let language = fromMaybe L.ENGLISH driver.language
        queryType = if coinsValue > 0 then CoinAdded else CoinSubtracted
        entityData = Notify.CoinsNotificationData {coins = coinsValue, event = eventFunction}
     in MTQuery.findByErrorAndLanguage (T.pack (show queryType)) language >>= processMessage driver entityData
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
    Just _ -> void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral incrementValue)
    Nothing -> setValidRideCountByDriverIdKey driverId expirationPeriod incrementValue

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey driverId date = "DriverCoinBalance:DriverId:" <> driverId.getId <> ":" <> date

getCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey driverId currentDate = Hedis.withCrossAppRedis $ Hedis.get (mkCoinAccumulationByDriverIdKey driverId currentDate)

setCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> Int -> Int -> m ()
setCoinAccumulationByDriverIdKey driverId currentDate count expirationPeriod = do
  void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral count)
  Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod

mkValidRideCountByDriverIdKey :: Id DP.Person -> Text
mkValidRideCountByDriverIdKey driverId = "DriverValidRideCount:DriverId:" <> driverId.getId

getValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m (Maybe Int)
getValidRideCountByDriverIdKey driverId = Hedis.withCrossAppRedis $ Hedis.get (mkValidRideCountByDriverIdKey driverId)

setValidRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> Int -> m ()
setValidRideCountByDriverIdKey driverId expirationPeriod count = do
  void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkValidRideCountByDriverIdKey driverId) (fromIntegral count)
  Hedis.withCrossAppRedis $ Hedis.expire (mkValidRideCountByDriverIdKey driverId) expirationPeriod

safeIncrBy :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Integer -> Id DP.Person -> Seconds -> m ()
safeIncrBy key value driverId timeDiffFromUtc = do
  _ <- getCoinsByDriverId driverId timeDiffFromUtc
  void $ Hedis.withCrossAppRedis $ Hedis.incrby key value

mkMetroRideCountByDriverIdKey :: Id DP.Person -> DCT.MetroRideType -> Text
mkMetroRideCountByDriverIdKey driverId metroRideType = "DriverMetroRideCount:DriverId:" <> driverId.getId <> ":MetroRideType:" <> show metroRideType

getMetroRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> m (Maybe Int)
getMetroRideCountByDriverIdKey driverId metroRideType = Hedis.withCrossAppRedis $ Hedis.get (mkMetroRideCountByDriverIdKey driverId metroRideType)

setMetroRideCountByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> Int -> Int -> m ()
setMetroRideCountByDriverIdKey driverId metroRideType expirationPeriod count = do
  void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkMetroRideCountByDriverIdKey driverId metroRideType) (fromIntegral count)
  Hedis.withCrossAppRedis $ Hedis.expire (mkMetroRideCountByDriverIdKey driverId metroRideType) expirationPeriod

incrementMetroRideCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> DCT.MetroRideType -> Int -> Int -> m ()
incrementMetroRideCount driverId metroRideType expirationPeriod incrementValue = do
  metroRideCountKeyExists <- getMetroRideCountByDriverIdKey driverId metroRideType
  case metroRideCountKeyExists of
    Just _ -> void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkMetroRideCountByDriverIdKey driverId metroRideType) (fromIntegral incrementValue)
    Nothing -> setMetroRideCountByDriverIdKey driverId metroRideType expirationPeriod incrementValue
