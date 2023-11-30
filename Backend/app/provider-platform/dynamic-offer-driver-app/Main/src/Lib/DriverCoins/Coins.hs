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
  )
where

import Data.Time (UTCTime (utctDay))
import Domain.Types.Coins.CoinHistory (CoinStatus (..))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Storage.CachedQueries.Merchant.TransporterConfig as TC
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.Coins.CoinsConfig as DCQ
import qualified Storage.Queries.Person as Person
import Storage.Queries.Ride as Ride

type EventFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, MonadReader r m, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters))

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey driverId date = "DriverCoinBalance:DriverId:" <> driverId.getId <> ":" <> date

getCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey driverId currentDate = Hedis.get (mkCoinAccumulationByDriverIdKey driverId currentDate)

setCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> Int -> m ()
setCoinAccumulationByDriverIdKey driverId currentDate count = do
  void $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral count)
  Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) 86400

udpateCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Int -> m ()
udpateCoinsByDriverId driverId coinUpdateValue = do
  now <- getCurrentTime
  let currentDate = show $ utctDay now
  void $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral coinUpdateValue)

getCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m Int
getCoinsByDriverId driverId = do
  now <- getCurrentTime
  let currentDate = show $ utctDay now
  coinKeyExists <- getCoinAccumulationByDriverIdKey driverId currentDate
  case coinKeyExists of
    Just coinBalance -> pure coinBalance
    Nothing -> do
      totalCoins <- CHistory.getTotalCoins driverId
      let coinBalance = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) totalCoins
      Hedis.whenWithLockRedis (mkCoinAccumulationByDriverIdKey driverId currentDate) 60 $ do
        setCoinAccumulationByDriverIdKey driverId currentDate coinBalance
      pure coinBalance

driverCoinsEvent :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsEventType -> m ()
driverCoinsEvent driverId merchantId merchantOpCityId eventType = do
  logDebug $ "Driver Coins Event Triggered for merchantOpCityId - " <> merchantOpCityId.getId <> " and driverId - " <> driverId.getId
  coinConfiguration <- DCQ.fetchFunctionsOnEventbasis eventType merchantId merchantOpCityId
  finalCoinsValue <- sum <$> forM coinConfiguration (\cc -> calculateCoins eventType driverId merchantId merchantOpCityId cc.eventFunction cc.expirationAt cc.coins)
  updateDriverCoins driverId finalCoinsValue

updateDriverCoins :: EventFlow m r => Id DP.Person -> Int -> m ()
updateDriverCoins driverId finalCoinsValue = do
  driver <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  void $ Person.updateTotalEarnedCoins driverId (finalCoinsValue + driver.totalEarnedCoins)
  udpateCoinsByDriverId driverId finalCoinsValue

calculateCoins :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
calculateCoins eventType driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins = do
  case eventType of
    DCT.Rating {..} -> handleRating driverId merchantId merchantOpCityId ratingValue chargeableDistance eventFunction mbexpirationTime numCoins
    DCT.EndRide {..} -> handleEndRide driverId merchantId merchantOpCityId isDisabled chargeableDistance_ eventFunction mbexpirationTime numCoins
    DCT.DriverToCustomerReferral {..} -> handleDriverReferral driverId merchantId merchantOpCityId chargeableDistance eventFunction mbexpirationTime numCoins
    DCT.Cancellation {..} -> handleCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup eventFunction mbexpirationTime numCoins
    _ -> pure 0

handleRating :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Int -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
handleRating driverId merchantId merchantOpCityId ratingValue chargeableDistance eventFunction mbexpirationTime numCoins = do
  logDebug $ "Driver Coins Handle Rating Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.OneOrTwoStarRating ->
      runActionWhenValidConditions
        [ pure (ratingValue == 1 || ratingValue == 2),
          checkHasTakenValidRide chargeableDistance
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    DCT.FiveStarRating ->
      runActionWhenValidConditions
        [ pure $ ratingValue == 5,
          checkHasTakenValidRide chargeableDistance
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    _ -> pure 0

handleEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
handleEndRide driverId merchantId merchantOpCityId isDisabled chargeableDistance_ eventFunction mbexpirationTime numCoins = do
  logDebug $ "Driver Coins Handle EndRide Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.RideCompleted ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide (Just chargeableDistance_)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    DCT.EightPlusRidesInOneDay -> do
      now <- getCurrentTime
      transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      let dateInIst = addUTCTime (-1 * secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) now
      rideList <- Ride.findTotalRidesInDay driverId dateInIst
      let totalRides = length rideList
      runActionWhenValidConditions
        [ pure (totalRides == 9),
          checkHasTakenValidRide (Just chargeableDistance_)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    DCT.PurpleRideCompleted ->
      runActionWhenValidConditions
        [ pure isDisabled,
          checkHasTakenValidRide (Just chargeableDistance_)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    _ -> pure 0

handleDriverReferral :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
handleDriverReferral driverId merchantId merchantOpCityId chargeableDistance eventFunction mbexpirationTime numCoins = do
  logDebug $ "Driver Coins Handle Referral Event Triggered - " <> show eventFunction
  case eventFunction of
    DCT.DriverReferral ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide chargeableDistance
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
    _ -> pure 0

handleCancellation :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Maybe Meters -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
handleCancellation driverId merchantId merchantOpCityId rideStartTime intialDisToPickup cancellationDisToPickup eventFunction mbexpirationTime numCoins = do
  logDebug $ "Driver Coins Handle Cancellation Event Triggered - " <> show eventFunction
  now <- getCurrentTime
  let timeDiff = diffUTCTime now rideStartTime
  transporterConfig <- TC.findByMerchantOpCityId merchantOpCityId >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case eventFunction of
    DCT.BookingCancellation -> do
      let validConditions = case (intialDisToPickup, cancellationDisToPickup) of
            (Just intialDis, Just cancellationDis) ->
              timeDiff > transporterConfig.cancellationTimeDiff && abs (intialDis - cancellationDis) < fromIntegral transporterConfig.cancellationDistDiff
            _ -> False
      runActionWhenValidConditions [pure validConditions] $ updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins
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

updateEventAndGetCoinsvalue :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> DCT.DriverCoinsFunctionType -> Maybe Int -> Int -> m Int
updateEventAndGetCoinsvalue driverId merchantId merchantOpCityId eventFunction mbexpirationTime numCoins = do
  now <- getCurrentTime
  uuid <- generateGUIDText
  -- Extract the integer value from maybeExpirationTime
  let expiryTime = (\expirationTime -> addUTCTime (fromIntegral expirationTime) now) <$> mbexpirationTime
  let status_ = if numCoins > 0 then Remaining else Used
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
            expirationAt = expiryTime,
            coinsUsed = 0
          }
  CHistory.updateCoinEvent driverCoinEvent
  pure numCoins

checkHasTakenValidRide :: (MonadReader r m, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)) => Maybe Meters -> m Bool
checkHasTakenValidRide chargeableDistance = do
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  pure $ case minTripDistanceForReferralCfg of
    Just distance ->
      case fmap metersToHighPrecMeters chargeableDistance of
        Just meters -> meters >= distance
        Nothing -> False
    Nothing -> False
