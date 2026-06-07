{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Pricing where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text as T hiding (elem, find, length, null, zip)
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Tools.Maps

getDistanceBin :: Int -> Text
getDistanceBin distance
  | distance < 0 = "unknown"
  | distance < 2000 = "0-2"
  | distance < 4000 = "2-4"
  | distance < 6000 = "4-6"
  | distance < 8000 = "6-8"
  | distance < 10000 = "8-10"
  | distance < 12000 = "10-12"
  | distance < 14000 = "12-14"
  | distance < 16000 = "14-16"
  | distance < 18000 = "16-18"
  | distance < 20000 = "18-20"
  | otherwise = "more_than_20"

-- Function to get the start of the 15-minute interval for a given time
getBucketStart :: UTCTime -> UTCTime
getBucketStart time =
  let -- Get the number of seconds since midnight
      secondsFromMidnight = getSecondsFromMidnight time
      -- Round down to nearest 15-minute mark (900 seconds = 15 minutes)
      intervalStart = (secondsFromMidnight `div` 900) * 900
      -- Return the UTCTime with the start of the interval
      (UTCTime day _) = time
   in UTCTime day (fromIntegral intervalStart)

-- Function to build the Redis key for a 15-minute interval
buildTimeBucket :: UTCTime -> T.Text
buildTimeBucket time = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" (getBucketStart time)

getSecondsFromMidnight :: UTCTime -> Integer
getSecondsFromMidnight (UTCTime _ diffTime) = floor diffTime

parseTimeString :: String -> Maybe UTCTime
parseTimeString str = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" str :: Maybe UTCTime

mkDemandVehicleCategoryCity :: UTCTime -> Maybe DVC.VehicleCategory -> Text -> Text
mkDemandVehicleCategoryCity time vehicleCategory cityId = "demand_VC_" <> show vehicleCategory <> "_cityId_" <> cityId <> "_time_" <> (buildTimeBucket time)

mkAcceptanceVehicleCategoryCity :: UTCTime -> Maybe DVC.VehicleCategory -> Text -> Text
mkAcceptanceVehicleCategoryCity time vehicleCategory cityId = "acceptance_VC_" <> show vehicleCategory <> "_cityId_" <> cityId <> "_time_" <> (buildTimeBucket time)

mkDemandVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkDemandVehicleCategory time vehicleCategory = "demand_{" <> show vehicleCategory <> "}_time_" <> buildTimeBucket time

mkAcceptanceVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkAcceptanceVehicleCategory time vehicleCategory = "acceptance_{" <> show vehicleCategory <> "}_time_" <> buildTimeBucket time

mkDemandVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkDemandVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "demand_{" <> show vehicleCategory <> "}_dB_" <> getDistanceBin distance <> "_time_" <> buildTimeBucket time
mkDemandVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkDemandVehicleCategory time vehicleCategory

mkAcceptanceVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkAcceptanceVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "acceptance_{" <> show vehicleCategory <> "}_dB_" <> getDistanceBin distance <> "_time_" <> buildTimeBucket time
mkAcceptanceVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkAcceptanceVehicleCategory time vehicleCategory

-- Adds a geo-spatial entry for the demand/acceptance event used by
-- 'getActualQAR'. Writes to three tiers: distance-bin, vehicle-category,
-- and city (city uses an atomic counter; the first two use geoAdd).
-- Member names are trimmed to 6 characters to keep sorted-set sizes small.
-- Each key is given a 1h TTL so stale time buckets self-expire.
-- Intended to be run inside a 'fork' off the hot search/quote path.
geoAddDynamicPricingCounter ::
  (MonadFlow m, CacheFlow m r) =>
  (UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text) ->
  (UTCTime -> Maybe DVC.VehicleCategory -> Text) ->
  (UTCTime -> Maybe DVC.VehicleCategory -> Text -> Text) ->
  UTCTime ->
  Maybe DVC.VehicleCategory ->
  Double ->
  Double ->
  Text ->
  Maybe Int ->
  Text ->
  m ()
geoAddDynamicPricingCounter mkDistBinKey mkVehCatKey mkCityKey time vehicleCategory lat lon memberId mbDistance cityId = do
  let member = BS8.pack $ T.unpack $ T.take 6 memberId
  -- City level: incr counter (no geo-spatial locality needed)
  incrWithTtl $ mkCityKey time vehicleCategory cityId
  -- Vehicle category level: geoAdd
  geoAddWithTtl (mkVehCatKey time vehicleCategory) lon lat member
  -- Distance bin level: geoAdd (when distance is known)
  whenJust mbDistance $ \_ ->
    geoAddWithTtl (mkDistBinKey time vehicleCategory mbDistance) lon lat member
  where
    incrWithTtl key = Hedis.withCrossAppRedis $ do
      void $ Hedis.incr key
      void $ Hedis.expire key 3600
    geoAddWithTtl key ln lt mem = Hedis.withCrossAppRedis $ do
      void $ Hedis.geoAdd key [(ln, lt, mem)]
      void $ Hedis.expire key 3600

-- Batched geoSearch via bulkShardedRedisBatch: groups keys by cluster slot
-- (the '{vehicleCategory}' hash tag ensures all keys for the same category
-- land on one slot) and returns the count of members within the radius for
-- each key. This avoids parsing individual member names — only 'length' is
-- used on the raw ByteString list returned by geoSearch.
batchGeoSearchCounts ::
  (MonadFlow m, CacheFlow m r) =>
  [Text] ->
  LatLong ->
  Double ->
  m [Int]
batchGeoSearchCounts keys location radius =
  Hedis.bulkShardedRedisBatch
    (\k -> k)
    ( \batchKeys ->
        forM batchKeys $ \key ->
          length <$> Hedis.withCrossAppRedis (Hedis.geoSearch key (Hedis.FromLonLat location.lon location.lat) (Hedis.ByRadius radius "km"))
    )
    keys

mkCongestionVehicleCategory :: UTCTime -> Maybe DVC.VehicleCategory -> Text
mkCongestionVehicleCategory time vehicleCategory = "congestion_VC_" <> show vehicleCategory <> "_time_" <> (buildTimeBucket time)

mkCongestionVehicleCategoryWithDistanceBin :: UTCTime -> Maybe DVC.VehicleCategory -> Maybe Int -> Text
mkCongestionVehicleCategoryWithDistanceBin time vehicleCategory (Just distance) = "congestion_VC_" <> show vehicleCategory <> "_dB_" <> (getDistanceBin distance) <> "_time_" <> (buildTimeBucket time)
mkCongestionVehicleCategoryWithDistanceBin time vehicleCategory Nothing = mkCongestionVehicleCategory time vehicleCategory

getActualQAR ::
  (MonadFlow m, CacheFlow m r) =>
  UTCTime ->
  Maybe DVC.VehicleCategory ->
  LatLong ->
  Double ->
  Int ->
  Text ->
  m (Maybe Double, Maybe Double)
getActualQAR now vehicleCategory location radius distance cityId = do
  let timeN = now
      timeN_1 = addUTCTime (-900) now
      timeN_2 = addUTCTime (-1800) now
      demDistN = mkDemandVehicleCategoryWithDistanceBin timeN vehicleCategory (Just distance)
      demDistN_1 = mkDemandVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
      demDistN_2 = mkDemandVehicleCategoryWithDistanceBin timeN_2 vehicleCategory (Just distance)
      accDistN = mkAcceptanceVehicleCategoryWithDistanceBin timeN vehicleCategory (Just distance)
      accDistN_1 = mkAcceptanceVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
      accDistN_2 = mkAcceptanceVehicleCategoryWithDistanceBin timeN_2 vehicleCategory (Just distance)
      demVcN = mkDemandVehicleCategory timeN vehicleCategory
      demVcN_1 = mkDemandVehicleCategory timeN_1 vehicleCategory
      demVcN_2 = mkDemandVehicleCategory timeN_2 vehicleCategory
      accVcN = mkAcceptanceVehicleCategory timeN vehicleCategory
      accVcN_1 = mkAcceptanceVehicleCategory timeN_1 vehicleCategory
      accVcN_2 = mkAcceptanceVehicleCategory timeN_2 vehicleCategory
      radiusKeys = [demDistN, demDistN_1, demDistN_2, accDistN, accDistN_1, accDistN_2, demVcN, demVcN_1, demVcN_2, accVcN, accVcN_1, accVcN_2]
  radiusCounts <- batchGeoSearchCounts radiusKeys location radius
  let countsMap = Map.fromList (zip radiusKeys radiusCounts)
      cnt k = Map.findWithDefault 0 k countsMap
      distCurrent = computeQAR (cnt demDistN + cnt demDistN_1) (cnt accDistN + cnt accDistN_1)
      distPast = computeQAR (cnt demDistN_1 + cnt demDistN_2) (cnt accDistN_1 + cnt accDistN_2)
      vcCurrent = computeQAR (cnt demVcN + cnt demVcN_1) (cnt accVcN + cnt accVcN_1)
      vcPast = computeQAR (cnt demVcN_1 + cnt demVcN_2) (cnt accVcN_1 + cnt accVcN_2)
  vc2xCurrent <-
    if isJust (distCurrent <|> vcCurrent)
      then pure Nothing
      else do
        twoXCounts <- batchGeoSearchCounts [demVcN, demVcN_1, accVcN, accVcN_1] location (2 * radius)
        let (d, d1, a, a1) = case twoXCounts of
              [w, x, y, z] -> (w, x, y, z)
              _ -> (0, 0, 0, 0)
        pure $ computeQAR (d + d1) (a + a1)
  (cityCurrent, cityPast) <-
    if isNothing (distCurrent <|> vcCurrent <|> vc2xCurrent) || isNothing (distPast <|> vcPast)
      then getCityLevelQAR timeN timeN_1 timeN_2 vehicleCategory cityId
      else pure (Nothing, Nothing)
  let currentQAR = distCurrent <|> vcCurrent <|> vc2xCurrent <|> cityCurrent
      pastQAR = distPast <|> vcPast <|> cityPast
  return (currentQAR, pastQAR)
  where
    computeQAR demandCount acceptanceCount
      | demandCount > 4 = Just (fromIntegral acceptanceCount / fromIntegral demandCount)
      | otherwise = Nothing

    getCityLevelQAR timeN timeN_1 timeN_2 vc cid = do
      let demCityN = mkDemandVehicleCategoryCity timeN vc cid
          demCityN_1 = mkDemandVehicleCategoryCity timeN_1 vc cid
          demCityN_2 = mkDemandVehicleCategoryCity timeN_2 vc cid
          accCityN = mkAcceptanceVehicleCategoryCity timeN vc cid
          accCityN_1 = mkAcceptanceVehicleCategoryCity timeN_1 vc cid
          accCityN_2 = mkAcceptanceVehicleCategoryCity timeN_2 vc cid
          cityKeys = [demCityN, demCityN_1, demCityN_2, accCityN, accCityN_1, accCityN_2]
      results <- Hedis.withCrossAppRedis $ Hedis.mGetClusterWithKeys @Int cityKeys
      let resultsMap = Map.fromList results
          c k = fromMaybe 0 (Map.lookup k resultsMap)
          cityCurrent = computeQAR (c demCityN + c demCityN_1) (c accCityN + c accCityN_1)
          cityPast = computeQAR (c demCityN_1 + c demCityN_2) (c accCityN_1 + c accCityN_2)
      return (cityCurrent, cityPast)
