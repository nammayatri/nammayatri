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
import Data.Text as T hiding (elem, find, length, map, null, zip)
import Data.Time hiding (getCurrentTime)
import qualified Database.Redis as Redis
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

-- Counts in-radius members for many keys in a SINGLE pipelined round-trip.
--
-- GEOSEARCH is a single-key command, so there is no multi-key form to batch
-- (unlike MGET) — but all keys for one call share the '{vehicleCategory}' hash
-- tag, hence one cluster slot. We therefore issue every GEOSEARCH inside ONE
-- 'runHedisEither' block: hedis auto-pipelines commands within a single Redis
-- monad action, so the whole batch costs one TCP round-trip to the owning node
-- instead of N sequential ones (the previous 'bulkShardedRedisBatch' wrapped
-- each geoSearch in its own 'withCrossAppRedis', so it neither pipelined nor —
-- with all keys on one slot — gained any parallelism, and it scrambled order).
--
-- Returns the in-radius count for each key, positionally aligned with the input
-- list (result[i] is the count for keys[i]) — callers index by position, so the
-- keys themselves aren't echoed back. Member names are not parsed — only 'length'
-- of the raw reply is used. On any reply/connection error the whole batch degrades
-- to zero counts (mirrors 'Hedis.geoSearch').
-- PRECONDITION: all keys must hash to the same cluster slot.
batchGeoSearchCounts ::
  (MonadFlow m, CacheFlow m r) =>
  [Text] ->
  LatLong ->
  Double ->
  m [Int]
batchGeoSearchCounts [] _ _ = pure []
batchGeoSearchCounts keys location radius = Hedis.withCrossAppRedis $ do
  migrating <- asks (.hedisMigrationStage)
  prefKeys <- mapM Hedis.buildKey keys
  let from = Hedis.FromLonLat location.lon location.lat
      by = Hedis.ByRadius radius "km"
      -- All commands run in one Redis action → hedis pipelines them.
      pipeline = Right <$> forM prefKeys (\pk -> Redis.geosearch pk from by)
      runner = if migrating then Hedis.runHedisEither' else Hedis.runHedisEither
  eRes <- withTryCatch "batchGeoSearchCounts" (runner pipeline)
  let zeros = map (const 0) keys
  case eRes of
    Left ex -> logWarning ("batchGeoSearchCounts pipeline threw, defaulting to zeros: " <> show ex) $> zeros
    Right (Left err) -> logWarning ("batchGeoSearchCounts reply error, defaulting to zeros: " <> show err) $> zeros
    Right (Right perKey) -> pure $ map (length . either (const []) identity) perKey

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
  -- Levels, finest first: distance-bin @ radius → vehicle-category @ radius →
  -- vehicle-category @ 2×radius → city. 'current' and 'past' resolve INDEPENDENTLY:
  -- each takes its value from the finest level at which it has enough demand, so the
  -- two may resolve at different levels. We descend only while either is unresolved,
  -- and at each level fetch ONLY the still-missing signal — current needs {N, N-1},
  -- past needs {N-1, N-2}, and only when both are missing do we fetch all 6 keys.
  let demDistAt t = mkDemandVehicleCategoryWithDistanceBin t vehicleCategory (Just distance)
      accDistAt t = mkAcceptanceVehicleCategoryWithDistanceBin t vehicleCategory (Just distance)
      demVcAt t = mkDemandVehicleCategory t vehicleCategory
      accVcAt t = mkAcceptanceVehicleCategory t vehicleCategory
      demCityAt t = mkDemandVehicleCategoryCity t vehicleCategory cityId
      accCityAt t = mkAcceptanceVehicleCategoryCity t vehicleCategory cityId
      geoFetch rad keys = batchGeoSearchCounts keys location rad
      -- City counters live in plain (non-geo) keys; align the reply to the requested
      -- key order and default missing keys to 0, matching 'batchGeoSearchCounts'.
      cityFetch keys = do
        results <- Hedis.withCrossAppRedis $ Hedis.mGetClusterWithKeys @Int keys
        let resMap = Map.fromList results
        pure $ map (\k -> fromMaybe 0 (Map.lookup k resMap)) keys
  -- Level 1: distance-bin @ radius (both unresolved → 6 keys).
  r1 <- resolveLevel demDistAt accDistAt (geoFetch radius) (Nothing, Nothing)
  if bothJust r1
    then return r1
    else do
      -- Level 2: vehicle-category @ radius.
      r2 <- resolveLevel demVcAt accVcAt (geoFetch radius) r1
      if bothJust r2
        then return r2
        else do
          -- Level 3: vehicle-category widened to 2× radius (same keys, larger search).
          r3 <- resolveLevel demVcAt accVcAt (geoFetch (2 * radius)) r2
          if bothJust r3
            then return r3
            else resolveLevel demCityAt accCityAt cityFetch r3 -- Level 4: city
  where
    timeN = now
    timeN_1 = addUTCTime (-900) now
    timeN_2 = addUTCTime (-1800) now

    bothJust (mbCur, mbPast) = isJust mbCur && isJust mbPast

    -- Fetch only the signal(s) still missing at this level and fill them in; a value
    -- already found at a finer level is preserved untouched. 'demAt'/'accAt' build a
    -- level's demand/acceptance key for a time bucket; 'fetch' returns counts aligned
    -- to the requested keys. current = QAR over {N, N-1}; past = QAR over {N-1, N-2}.
    resolveLevel demAt accAt fetch (mbCur, mbPast) =
      case (isNothing mbCur, isNothing mbPast) of
        (False, False) -> pure (mbCur, mbPast)
        (True, False) -> do
          cs <- fetch [demAt timeN, demAt timeN_1, accAt timeN, accAt timeN_1]
          pure (currentOf cs, mbPast)
        (False, True) -> do
          cs <- fetch [demAt timeN_1, demAt timeN_2, accAt timeN_1, accAt timeN_2]
          pure (mbCur, pastOf cs)
        (True, True) -> do
          cs <- fetch [demAt timeN, demAt timeN_1, demAt timeN_2, accAt timeN, accAt timeN_1, accAt timeN_2]
          pure (toQAR cs)
      where
        currentOf cs = case cs of
          [dN, dN1, aN, aN1] -> computeQAR (dN + dN1) (aN + aN1)
          _ -> Nothing
        pastOf cs = case cs of
          [dN1, dN2, aN1, aN2] -> computeQAR (dN1 + dN2) (aN1 + aN2)
          _ -> Nothing

    -- (current, past) candidates from a 6-count reply ordered
    -- [demN, demN-1, demN-2, accN, accN-1, accN-2].
    toQAR counts = case counts of
      [dN, dN1, dN2, aN, aN1, aN2] ->
        (computeQAR (dN + dN1) (aN + aN1), computeQAR (dN1 + dN2) (aN1 + aN2))
      _ -> (Nothing, Nothing)

    computeQAR demandCount acceptanceCount
      | demandCount > 4 = Just (fromIntegral acceptanceCount / fromIntegral demandCount)
      | otherwise = Nothing
