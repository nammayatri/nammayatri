{-# OPTIONS_GHC -Wno-type-defaults #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib.DriverCoins.Coins
  ( driverCoinsEvent,
    mkCoinAccumulationByDriverIdKey,
    getCoinAccumulationByDriverIdKey,
    setCoinAccumulationByDriverIdKey,
  )
where

import Data.Time (UTCTime (utctDay))
import Domain.Types.Coins.CoinHistory (CoinStatus (..))
import qualified Domain.Types.Coins.CoinHistory as DTCC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverCoins.Types as DCT
import qualified Storage.Queries.Coins.CoinHistory as CHistory
import qualified Storage.Queries.Coins.DriverCoins as DCQ
import qualified Storage.Queries.Person as Person
import Storage.Queries.Ride as Ride

type EventFlow m r = (CacheFlow m r, MonadFlow m, MonadReader r m, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters))

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey driverId date = "DriverCoinBalance:DriverId:" <> driverId.getId <> ":" <> date

getCoinAccumulationByDriverIdKey :: CacheFlow m r => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey driverId currentDate = Hedis.get (mkCoinAccumulationByDriverIdKey driverId currentDate)

setCoinAccumulationByDriverIdKey :: CacheFlow m r => Id DP.Person -> Text -> Int -> m ()
setCoinAccumulationByDriverIdKey driverId currentDate count = do
  Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral count)
  Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) 86400

getCoinsByDriveId :: MonadFlow m => Id DP.Person -> m Int
getCoinsByDriveId driverId = do
  totalCoins <- CHistory.getTotalCoins driverId
  let total = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) totalCoins
  return total

driverCoinsEvent :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> DCT.DriverCoinsEventType -> m ()
driverCoinsEvent driverId merchantId eventType = do
  logDebug "Driver Coins Event Triggered"
  coinConfiguration <- DCQ.fetchFunctionsOnEventbasis eventType merchantId
  let eventFunctionsWithTime = map (\cc -> (cc.fn, cc.expirationAt)) coinConfiguration
  coinValues <- forM eventFunctionsWithTime $ uncurry (calculateCoins eventType driverId merchantId)
  let finalCoinsValue = sum coinValues
  driver <- Person.getTotalEarnedAndUsedCoins driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  void $ Person.updateTotalEarnedCoins driverId (finalCoinsValue + driver.totalEarnedCoins)
  now <- getCurrentTime
  let currentDate = show $ utctDay now
  coinKeyExists <- getCoinAccumulationByDriverIdKey driverId currentDate
  case coinKeyExists of
    Just _ -> void $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral finalCoinsValue)
    Nothing -> do
      coinBalance <- getCoinsByDriveId driverId
      setCoinAccumulationByDriverIdKey driverId currentDate coinBalance

calculateCoins :: EventFlow m r => DCT.DriverCoinsEventType -> Id DP.Person -> Id DM.Merchant -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
calculateCoins eventType driverId merchantId eventFunction mbexpirationTime = do
  case eventType of
    DCT.Rating {..} -> handleRating driverId merchantId ratingValue chargeableDistance eventFunction mbexpirationTime
    DCT.EndRide {..} -> handleEndRide driverId merchantId isDisabled chargeableDistance_ eventFunction mbexpirationTime
    DCT.Referral {..} -> handleReferral driverId merchantId _type chargeableDistance eventFunction mbexpirationTime
    DCT.Cancellation {..} -> handleCancellation driverId merchantId rideStartTime intialDisToPickup cancellationDisToPickup eventFunction mbexpirationTime
    _ -> pure 0

handleRating :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Int -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
handleRating driverId merchantId ratingValue chargeableDistance ratingFn mbexpirationTime = do
  logDebug "Driver Coins Handle Rating Event Triggered"
  case ratingFn of
    DCT.OneOrTwoStarRating ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide chargeableDistance,
          pure (ratingValue == 1 || ratingValue == 2)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    DCT.FiveStarRating ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide chargeableDistance,
          pure $ ratingValue == 5
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    _ -> pure 0

handleEndRide :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Bool -> Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
handleEndRide driverId merchantId isDisabled chargeableDistance_ ratingFn mbexpirationTime = do
  logDebug "Driver Coins Handle EndRide Event Triggered"
  rideList <- Ride.findTotalRidesInDay driverId
  let totalRides = length rideList
  case ratingFn of
    DCT.RideCompleted ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide (Just chargeableDistance_)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    DCT.EightPlusRidesInOneDay ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide (Just chargeableDistance_),
          pure (totalRides > 8)
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    DCT.PurpleRideCompleted ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide (Just chargeableDistance_),
          pure isDisabled
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    _ -> pure 0

handleReferral :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> Text -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
handleReferral driverId merchantId _type chargeableDistance ratingFn mbexpirationTime = do
  logDebug "Driver Coins Handle Referral Event Triggered"
  case ratingFn of
    DCT.CustomerReferral ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide chargeableDistance
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    DCT.DriverReferral ->
      runActionWhenValidConditions
        [ checkHasTakenValidRide chargeableDistance
        ]
        $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
    _ -> pure 0

handleCancellation :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> UTCTime -> Maybe Meters -> Maybe Meters -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
handleCancellation driverId merchantId rideStartTime intialDisToPickup cancellationDisToPickup ratingFn mbexpirationTime = do
  logDebug "Driver Coins Handle Cancellation Event Triggered"
  now <- getCurrentTime
  let timeDiff = diffUTCTime now rideStartTime
  case ratingFn of
    DCT.BookingCancellation -> do
      let validConditions = case (intialDisToPickup, cancellationDisToPickup) of
            (Just intialDis, Just cancellationDis) ->
              timeDiff > 120 && abs (intialDis - cancellationDis) < 50
            _ -> False
      runActionWhenValidConditions [pure validConditions] $ updateEventAndGetCoinsvalue driverId merchantId ratingFn mbexpirationTime
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

updateEventAndGetCoinsvalue :: EventFlow m r => Id DP.Person -> Id DM.Merchant -> DCT.DriverCoinsFunctionType -> Maybe Int -> m Int
updateEventAndGetCoinsvalue driverId merchantId eventFunction mbexpirationTime = do
  coinsConfig <- DCQ.fetchCoins eventFunction merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let coinsValue = coinsConfig.coins
  now <- getCurrentTime
  uuid <- generateGUIDText
  -- Extract the integer value from maybeExpirationTime
  let expiryTime = (\expirationTime -> addUTCTime (fromIntegral expirationTime) now) <$> mbexpirationTime
  let status_ = if coinsValue > 0 then Remaining else Used
  let driverCoinEvent =
        DTCC.CoinHistory
          { id = uuid,
            driverId = driverId.getId,
            merchantId = merchantId.getId,
            fn = eventFunction,
            coins = coinsValue,
            status = status_,
            createdAt = now,
            expirationAt = expiryTime,
            coinsUsed = 0
          }
  CHistory.updateCoinEvent driverCoinEvent
  pure coinsValue

checkHasTakenValidRide :: (MonadReader r m, HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)) => Maybe Meters -> m Bool
checkHasTakenValidRide chargeableDistance = do
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  pure $ case minTripDistanceForReferralCfg of
    Just distance ->
      case fmap metersToHighPrecMeters chargeableDistance of
        Just meters -> meters >= distance
        Nothing -> False
    Nothing -> False
