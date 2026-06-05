{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Pricing where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Text as T hiding (elem, find, length, null)
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

-- | Stepwise QAR (Quote-Acceptance-Ratio) using geoSearch.
-- Hierarchy: distanceBin → vehicleCategory → city.
-- At each geo level, fetches N and N-1 first; if current window has enough
-- demand (>4), fetches N-2 to compute the past window (reusing N-1).
-- All geoSearch calls within a level are batched via 'bulkShardedRedisBatch'
-- so keys sharing the '{vehicleCategory}' hash tag pipeline to one slot.
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
  -- Step 1: DistanceBin level (N, N-1)
  let demDistN = mkDemandVehicleCategoryWithDistanceBin timeN vehicleCategory (Just distance)
      demDistN_1 = mkDemandVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
      accDistN = mkAcceptanceVehicleCategoryWithDistanceBin timeN vehicleCategory (Just distance)
      accDistN_1 = mkAcceptanceVehicleCategoryWithDistanceBin timeN_1 vehicleCategory (Just distance)
  distCounts <- batchGeoSearchCounts [demDistN, demDistN_1, accDistN, accDistN_1] location radius
  let (cDemDistN, cDemDistN_1, cAccDistN, cAccDistN_1) = case distCounts of
        [a, b, c, d] -> (a, b, c, d)
        _ -> (0, 0, 0, 0)
      distCurrent = computeQAR (cDemDistN + cDemDistN_1) (cAccDistN + cAccDistN_1)
  case distCurrent of
    Just _ -> do
      -- Fetch N-2 for past (N-1 already known)
      let demDistN_2 = mkDemandVehicleCategoryWithDistanceBin timeN_2 vehicleCategory (Just distance)
          accDistN_2 = mkAcceptanceVehicleCategoryWithDistanceBin timeN_2 vehicleCategory (Just distance)
      pastCounts <- batchGeoSearchCounts [demDistN_2, accDistN_2] location radius
      let (cDemDistN_2, cAccDistN_2) = case pastCounts of
            [a, b] -> (a, b)
            _ -> (0, 0)
          distPast = computeQAR (cDemDistN_1 + cDemDistN_2) (cAccDistN_1 + cAccDistN_2)
      return (distCurrent, distPast)
    Nothing -> do
      -- Step 2: VehicleCategory level (N, N-1)
      let demVcN = mkDemandVehicleCategory timeN vehicleCategory
          demVcN_1 = mkDemandVehicleCategory timeN_1 vehicleCategory
          accVcN = mkAcceptanceVehicleCategory timeN vehicleCategory
          accVcN_1 = mkAcceptanceVehicleCategory timeN_1 vehicleCategory
      vcCounts <- batchGeoSearchCounts [demVcN, demVcN_1, accVcN, accVcN_1] location radius
      let (cDemVcN, cDemVcN_1, cAccVcN, cAccVcN_1) = case vcCounts of
            [a, b, c, d] -> (a, b, c, d)
            _ -> (0, 0, 0, 0)
          vcCurrent = computeQAR (cDemVcN + cDemVcN_1) (cAccVcN + cAccVcN_1)
      case vcCurrent of
        Just _ -> do
          -- Fetch N-2 for past
          let demVcN_2 = mkDemandVehicleCategory timeN_2 vehicleCategory
              accVcN_2 = mkAcceptanceVehicleCategory timeN_2 vehicleCategory
          vcPastCounts <- batchGeoSearchCounts [demVcN_2, accVcN_2] location radius
          let (cDemVcN_2, cAccVcN_2) = case vcPastCounts of
                [a, b] -> (a, b)
                _ -> (0, 0)
              vcPast = computeQAR (cDemVcN_1 + cDemVcN_2) (cAccVcN_1 + cAccVcN_2)
          return (vcCurrent, vcPast)
        Nothing -> do
          -- Step 3: City level (incr counters, no geo-spatial locality)
          getCityLevelQAR timeN timeN_1 timeN_2 vehicleCategory cityId
  where
    computeQAR demandCount acceptanceCount
      | demandCount > 4 = Just (fromIntegral acceptanceCount / fromIntegral demandCount)
      | otherwise = Nothing

    getCityLevelQAR timeN timeN_1 timeN_2 vc cid = do
      let demCityN = mkDemandVehicleCategoryCity timeN vc cid
          demCityN_1 = mkDemandVehicleCategoryCity timeN_1 vc cid
          accCityN = mkAcceptanceVehicleCategoryCity timeN vc cid
          accCityN_1 = mkAcceptanceVehicleCategoryCity timeN_1 vc cid
      results <- Hedis.withCrossAppRedis $ Hedis.mGetClusterWithKeys @Int [demCityN, demCityN_1, accCityN, accCityN_1]
      let resultsMap = Map.fromList results
          demN = Map.lookup demCityN resultsMap
          demN_1 = Map.lookup demCityN_1 resultsMap
          accN = Map.lookup accCityN resultsMap
          accN_1 = Map.lookup accCityN_1 resultsMap
      let cityCurrent = computeQAR (fromMaybe 0 demN + fromMaybe 0 demN_1) (fromMaybe 0 accN + fromMaybe 0 accN_1)
      case cityCurrent of
        Just _ -> do
          let demCityN_2 = mkDemandVehicleCategoryCity timeN_2 vc cid
              accCityN_2 = mkAcceptanceVehicleCategoryCity timeN_2 vc cid
          results2 <- Hedis.withCrossAppRedis $ Hedis.mGetClusterWithKeys @Int [demCityN_2, accCityN_2]
          let resultsMap2 = Map.fromList results2
              demN_2 = Map.lookup demCityN_2 resultsMap2
              accN_2 = Map.lookup accCityN_2 resultsMap2
          let cityPast = computeQAR (fromMaybe 0 demN_1 + fromMaybe 0 demN_2) (fromMaybe 0 accN_1 + fromMaybe 0 accN_2)
          return (cityCurrent, cityPast)
        Nothing -> return (Nothing, Nothing)
