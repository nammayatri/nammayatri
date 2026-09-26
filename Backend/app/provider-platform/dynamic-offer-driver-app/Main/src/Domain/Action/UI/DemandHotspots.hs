module Domain.Action.UI.DemandHotspots
  ( getDriverDemandHotspots,
    updateDemandHotspotsOnSearch,
    updateDemandHotspotsOnBooking,
    getActiveSearchLocations,
  )
where

import API.Types.UI.DemandHotspots
import Data.Aeson (withArray)
import qualified Data.Aeson as Ae
import qualified Data.Geohash as Geohash
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Time as T
import qualified Data.Vector as V
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.TransporterConfig
import Domain.Types.VehicleVariant (castVariantToServiceTier, castVehicleVariantToVehicleCategory)
import Environment
import EulerHS.Prelude hiding (foldr', id, length, map, mapM_, maximumBy, null, sum, whenJust)
import GHC.Num.Integer (integerToInt)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.Error
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Time (utcToMilliseconds)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.FarePolicy (getSyntheticCongestionMultiplier)
import SharedLogic.Pricing (getDistanceBin)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Vehicle as QVehicle

-- What it is:
-- Entire city is divided into geohashes, whenever we encounter a search, we increase the frequency and whenver a booking happens
-- we decrease the frequency, so hotpsots depics unserved searches in that geohash.
--
-- How it works:
--
-- Redis data structures we are using - set and sorted set
-- Set - For storing all geohashes we have in a city
-- Sorted Set - Storing searches categorised by cityId and geohash
--
-- On Search - We add a uuid to the sorted set of that city and geohash
-- On Booking - We reomve the uuid with highest score from sorted set of that city and geohash

newtype HotspotObject = HotspotObject (Text, Double, Double, Maybe Int, Maybe Int)
  -- (first 6 chars of SearchRequestId, Lat, Long, distanceMeters, durationSeconds)
  deriving (Show)

instance ToJSON HotspotObject where
  toJSON (HotspotObject (name, lat, long, mbDistanceMeters, mbDurationSeconds)) =
    toJSON [toJSON name, toJSON lat, toJSON long, toJSON mbDistanceMeters, toJSON mbDurationSeconds]

instance FromJSON HotspotObject where
  parseJSON = withArray "HotspotObject" $ \vec ->
    case V.length vec of
      5 ->
        HotspotObject
          <$> ( (,,,,) <$> parseJSON (vec V.! 0)
                  <*> parseJSON (vec V.! 1)
                  <*> parseJSON (vec V.! 2)
                  <*> parseJSON (vec V.! 3)
                  <*> parseJSON (vec V.! 4)
              )
      -- Legacy entries written before distance/duration tracking was added.
      -- Must stay accepted until they age out of the sorted set's own TTL/pruning,
      -- otherwise a single legacy member fails the whole geohash's decode.
      3 ->
        HotspotObject
          <$> ( (,,,,) <$> parseJSON (vec V.! 0)
                  <*> parseJSON (vec V.! 1)
                  <*> parseJSON (vec V.! 2)
                  <*> pure Nothing
                  <*> pure Nothing
              )
      _ -> fail "Expected an array of 3 (legacy) or 5 elements"

getDriverDemandHotspots ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow GetDemandHotspotsResp
  )
getDriverDemandHotspots (mbPersonId, _, merchantOpCityId) = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      if configs.enableDemandHotspots
        then do
          let cachedResultKey = mkDemandHotspotCachedKey merchantOpCityId.getId
          cachedResult :: Maybe GetDemandHotspotsResp <- Redis.safeGet cachedResultKey
          res <- case cachedResult of
            Just res -> do
              fork "Calculating demand hotpspots" $ do
                expirySec <- Redis.ttl cachedResultKey
                when (expirySec < 60) $ do
                  void $ calculateDemandHotspots configs cachedResultKey True
              return res
            Nothing -> calculateDemandHotspots configs cachedResultKey False
          overlayCongestionMultiplier configs transporterConfig.timeDiffFromUtc res
        else do
          throwError $ InvalidRequest "Demand Hotspots is not enabled"
    _ -> throwError $ InvalidRequest "Demand Hotspots feature configs not set"
  where
    -- 'frequency'/'location' are shared across every driver in the city (cached
    -- under a single per-city key), but the congestion multiplier is per
    -- ServiceTierType. So the shared, locked, TTL-cached computation below only
    -- ever produces 'multiplier = Nothing' placeholders -- the real value is
    -- overlaid fresh per request in 'overlayCongestionMultiplier', after the
    -- calling driver's own vehicle category and tier are known.
    calculateDemandHotspots configs cachedResultKey forceRecal =
      Redis.withWaitAndLockRedis (mkHotspotsCalculationLockKey merchantOpCityId.getId) 10 10000 $ do
        --1e4 microseconds
        cachedResult :: Maybe GetDemandHotspotsResp <- if forceRecal then return Nothing else Redis.safeGet cachedResultKey
        case cachedResult of
          Just res -> return res
          Nothing -> do
            activeGeohashes :: [Text] <- Redis.sMembers (mkGeohashSetKey merchantOpCityId.getId)
            now <- getCurrentTime
            let allSortedSetsKeysWithGeohash :: [(Text, Text)] = map (\gh -> (mkDemandHotspotSortedSetKey merchantOpCityId.getId gh, gh)) activeGeohashes
                expiryTimeForSSObjects = T.addUTCTime (- 1 * 60 * fromIntegral configs.analysisDurationMinutes) now
            mapM_ (\(key, _) -> Redis.zRemRangeByScore key 0 (utcToMilliseconds expiryTimeForSSObjects)) allSortedSetsKeysWithGeohash
            freqWithSortedSetKey :: [(Int, Text)] <- mapM getFrequencyWithSSKey allSortedSetsKeysWithGeohash
            finalResults :: [(Int, Maps.LatLong)] <- mapM calculateResult $ take configs.noOfGeohashesToReturn $ sortBy (flip compare) $ filter (\(freq, _) -> freq > 0) freqWithSortedSetKey
            let resp =
                  GetDemandHotspotsResp
                    { createdAt = now,
                      expiryAt = T.addUTCTime (60 * fromIntegral configs.resultDurationMinutes) now,
                      hotspotsDetails = map (\(freq, loc) -> HotspotsDetails {frequency = freq, location = loc, multiplier = Nothing}) finalResults
                    }
            Redis.setExp cachedResultKey resp (60 * configs.resultDurationMinutes)
            return resp

    getFrequencyWithSSKey (sortedSetKey, geohash) = do
      frequency <- fmap integerToInt (Redis.zCard sortedSetKey)
      when (frequency == 0) $ do
        void $ Redis.srem (mkGeohashSetKey merchantOpCityId.getId) [geohash]
      return (frequency, sortedSetKey)

    calculateResult (_, sortedSetKey) = do
      res <- Redis.zRange sortedSetKey 0 1000000
      members :: [HotspotObject] <- mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res
      let freq = fromIntegral $ length members
          (sumLat, sumLong) = foldr' (\(HotspotObject (_, lat, long, _, _)) (accLat, accLong) -> (accLat + lat, accLong + long)) (0, 0) members
          avgLatLong = LatLong (sumLat / freq) (sumLong / freq)
      return (floor freq, avgLatLong)

    -- Resolves the calling driver's own vehicle category and tier and overlays
    -- a congestion multiplier onto each returned hotspot. Cheap on the common
    -- path (one DB/replica read + up to noOfGeohashesToReturn short-TTL Redis
    -- GETs); the expensive part (deriving a representative trip + calling the
    -- pricing rule engine) only runs on a per-(geohash,serviceTier) cache miss,
    -- at most once per 'congestionMultiplierCacheTTLInMin' window.
    overlayCongestionMultiplier configs timeDiffFromUtc res = do
      mbCategoryAndTier <- resolveDriverVehicleCategoryAndServiceTier
      hotspotsWithMultiplier <- mapM (attachMultiplier configs timeDiffFromUtc mbCategoryAndTier) res.hotspotsDetails
      return res {hotspotsDetails = hotspotsWithMultiplier}

    -- Both derived from the driver's actual vehicle: 'castVehicleVariantToVehicleCategory'
    -- for the coarse category we cache by, 'castVariantToServiceTier' for the exact
    -- tier fed into the pricing rule engine (no guessing/default-tier fallback).
    resolveDriverVehicleCategoryAndServiceTier = case mbPersonId of
      Nothing -> pure Nothing
      Just personId -> do
        mbVehicle <- B.runInReplica $ QVehicle.findById personId
        pure $
          mbVehicle
            <&> \v ->
              ( fromMaybe (castVehicleVariantToVehicleCategory v.variant) v.category,
                castVariantToServiceTier v.variant
              )

    attachMultiplier configs timeDiffFromUtc mbCategoryAndTier hotspot = do
      multiplier <- lookupCongestionMultiplier configs timeDiffFromUtc mbCategoryAndTier hotspot.location
      return hotspot {multiplier = multiplier}

    -- Cache-first, then compute-and-cache under a lock. Deliberately a NEW,
    -- separate Redis namespace (not the old CongestionChargeAvg job's keys) --
    -- this value comes from a synthetic representative trip derived from
    -- recent search activity at this geohash, not a real booking, and we don't
    -- want it silently mixing into the (unused, but still-shipped) job's cache.
    lookupCongestionMultiplier configs timeDiffFromUtc mbCategoryAndTier latlong = do
      let mbServiceTier = snd <$> mbCategoryAndTier
          mbGeohash = T.pack <$> Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
      case mbGeohash of
        Nothing -> pure Nothing
        Just geohash -> do
          let cacheKey = mkHotspotCongestionCacheKey merchantOpCityId.getId geohash mbServiceTier
          mbCached :: Maybe Double <- Redis.safeGet cacheKey
          case mbCached of
            Just _ -> pure mbCached
            Nothing ->
              Redis.withWaitAndLockRedis (mkHotspotCongestionLockKey merchantOpCityId.getId geohash mbServiceTier) 5 5000 $ do
                mbCachedAgain :: Maybe Double <- Redis.safeGet cacheKey
                case mbCachedAgain of
                  Just _ -> pure mbCachedAgain
                  -- No resolved category/tier (unauthenticated or driver has no
                  -- vehicle record) -- can't price a synthetic trip without a
                  -- tier, so surface "no data" rather than guessing one.
                  Nothing -> case mbCategoryAndTier of
                    Nothing -> pure Nothing
                    Just (vehicleCat, serviceTier) -> do
                      mbTrip <- deriveRepresentativeTrip configs (mkDemandHotspotSortedSetKey merchantOpCityId.getId geohash)
                      case mbTrip of
                        Nothing -> pure Nothing
                        Just (distanceMeters, durationSeconds) -> do
                          mbMultiplier <- getSyntheticCongestionMultiplier merchantOpCityId timeDiffFromUtc geohash latlong (Just vehicleCat) serviceTier distanceMeters durationSeconds
                          whenJust mbMultiplier $ \m -> Redis.setExp cacheKey m (60 * fromMaybe 15 configs.congestionMultiplierCacheTTLInMin)
                          pure mbMultiplier

    -- Buckets a geohash's recent (already-pruned) HotspotObjects by real
    -- observed distance (reusing the same 2km bins the live fare-pricing path
    -- uses), picks the bin with the most members, and averages the *real*
    -- distanceMeters/durationSeconds of members in that bin -- a genuine
    -- "typical trip from here" signal, not an assumption. Only falls back to
    -- 'fallbackAvgSpeedKmph' if the winning bin has distance data but no
    -- member in it has duration data at all.
    deriveRepresentativeTrip configs sortedSetKey = do
      res <- Redis.zRange sortedSetKey 0 1000000
      members :: [HotspotObject] <- mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res
      let withDistance = [(getDistanceBin d, d, dur) | HotspotObject (_, _, _, Just d, dur) <- members]
          binCounts = Map.fromListWith (+) [(bin, 1 :: Int) | (bin, _, _) <- withDistance]
      if null binCounts
        then pure Nothing
        else do
          let winningBin = fst $ maximumBy (comparing snd) (Map.toList binCounts)
              inWinningBin = [(d, dur) | (bin, d, dur) <- withDistance, bin == winningBin]
              avgDistance = sum (map fst inWinningBin) `div` length inWinningBin
              durationsWithValue = mapMaybe snd inWinningBin
              avgDuration
                | null durationsWithValue = round (fromIntegral avgDistance / 1000 / fromMaybe 30.0 configs.fallbackAvgSpeedKmph * 3600)
                | otherwise = sum durationsWithValue `div` length durationsWithValue
          pure $ Just (avgDistance, avgDuration)

-- Keyed by the precise ServiceTierType, not just the coarse VehicleCategory:
-- 'getSyntheticCongestionMultiplier' feeds the exact tier into the pricing rule
-- engine, and its output can legitimately differ between tiers that share the
-- same category (e.g. SEDAN vs SUV are both VehicleCategory.CAR) -- caching by
-- category alone would silently serve one tier's multiplier to another.
mkHotspotCongestionCacheKey :: Text -> Text -> Maybe DVST.ServiceTierType -> Text
mkHotspotCongestionCacheKey cityId geohash serviceTier = "DH:CongestionMultiplier:cityId:" <> cityId <> ":geohash:" <> geohash <> "_serviceTier_" <> show serviceTier

mkHotspotCongestionLockKey :: Text -> Text -> Maybe DVST.ServiceTierType -> Text
mkHotspotCongestionLockKey cityId geohash serviceTier = "DH:CongestionLock:cityId:" <> cityId <> ":geohash:" <> geohash <> "_serviceTier_" <> show serviceTier

updateDemandHotspotsOnSearch :: Id SearchRequest -> Id MerchantOperatingCity -> TransporterConfig -> Maps.LatLong -> Maybe Meters -> Maybe Seconds -> Flow ()
updateDemandHotspotsOnSearch searchReqId merchantOpCityId transporterConfig latlong mbDistance mbDuration = do
  now <- getCurrentTime
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
              geohashSetKey = mkGeohashSetKey merchantOpCityId.getId
              expirationSecond = 60 * configs.analysisDurationMinutes
              object = HotspotObject (T.take 6 searchReqId.getId, latlong.lat, latlong.lon, (.getMeters) <$> mbDistance, (.getSeconds) <$> mbDuration)
          Redis.zAdd sortedSetKey [(utcToMilliseconds now, object)]
          Redis.expire sortedSetKey expirationSecond
          Redis.sAddExp geohashSetKey [geohash] expirationSecond
    _ -> logDebug "Demand hotspots not enabled or configs not set on search"

updateDemandHotspotsOnBooking ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id SearchRequest ->
  Id MerchantOperatingCity ->
  TransporterConfig ->
  Maps.LatLong ->
  Maybe Meters ->
  Maybe Seconds ->
  m ()
updateDemandHotspotsOnBooking searchReqId merchantOpCityId transporterConfig latlong mbDistance mbDuration = do
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
              -- Must match the object stored at search-time byte-for-byte (same
              -- distance/duration), or ZREM won't find it to remove on booking.
              object = HotspotObject (T.take 6 searchReqId.getId, latlong.lat, latlong.lon, (.getMeters) <$> mbDistance, (.getSeconds) <$> mbDuration)
          res <- Redis.zRem' sortedSetKey [object]
          logDebug $ "deleted members count " <> show res
    _ -> logDebug "Demand hotspots not enabled or configs not set on booking"

mkDemandHotspotCachedKey :: Text -> Text
mkDemandHotspotCachedKey opCityId = "DH:CK:cityId:" <> opCityId

mkGeohashSetKey :: Text -> Text
mkGeohashSetKey opCityId = "DH:GSK:cityId:" <> opCityId

mkDemandHotspotSortedSetKey :: Text -> Text -> Text
mkDemandHotspotSortedSetKey opCityId geohash = "DH:cityId:" <> opCityId <> "GH:" <> geohash

mkHotspotsCalculationLockKey :: Text -> Text
mkHotspotsCalculationLockKey opCityId = "DH:CalcLock:cityId:" <> opCityId

-- | Returns active (unserved) search locations near a given point within radiusMeters.
-- Only reads the geohash buckets that overlap the bounding box of the circle --
-- O(~9 buckets) regardless of total city searches.
-- Pure read path: no writes, no side effects.
getActiveSearchLocations ::
  Id MerchantOperatingCity ->
  Maps.LatLong -> -- driver center point
  Int -> -- radiusMeters
  Double -> -- cutoffScore: fromIntegral (utcToMilliseconds (now - stalenessMinutes)), pre-computed by caller
  Int -> -- geohash precision (from config)
  Flow [(Text, Double, Double)]
getActiveSearchLocations merchantOpCityId center radiusMeters cutoffScore geohashPrecision = do
  let candidateGeohashes = geohashCover center radiusMeters geohashPrecision
      centerLatLong = Maps.LatLong center.lat center.lon
  rawEntries <- concat <$> mapM processGeohash candidateGeohashes
  -- Filter by actual distance -- bucket boundaries are rectangular, circle is round
  pure $ filter (isWithinRadius centerLatLong radiusMeters) rawEntries
  where
    processGeohash geohash = do
      let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
      rawItems <- Redis.zRangeByScore sortedSetKey cutoffScore (1 / 0 :: Double)
      pure $ mapMaybe (\r -> (\(HotspotObject (name, lat, lon, _, _)) -> (name, lat, lon)) <$> (Ae.decode $ cs r)) rawItems

    isWithinRadius cl r (_, slat, slon) =
      highPrecMetersToMeters (distanceBetweenInMeters cl (Maps.LatLong slat slon)) <= fromIntegral r

-- | Returns the full set of geohash cells that cover the circle defined by
-- center + radiusMeters. Samples a grid of points across the bounding box
-- at intervals of half the cell width -- guarantees no interior cell is missed.
geohashCover :: Maps.LatLong -> Int -> Int -> [String]
geohashCover center radiusMeters precision =
  nub $ catMaybes [Geohash.encode precision (lat, lon) | lat <- latSteps, lon <- lonSteps]
  where
    -- approximate cell width at this precision (metres), step = half cell to ensure full cover
    cellWidthMeters = geohashCellWidthMeters precision
    stepMeters = cellWidthMeters / 2.0
    latDeltaTotal = fromIntegral radiusMeters / 111320.0
    lonDeltaTotal = fromIntegral radiusMeters / (111320.0 * cos (center.lat * pi / 180.0))
    stepLat = stepMeters / 111320.0
    stepLon = stepMeters / (111320.0 * cos (center.lat * pi / 180.0))
    latSteps = [center.lat - latDeltaTotal, center.lat - latDeltaTotal + stepLat .. center.lat + latDeltaTotal]
    lonSteps = [center.lon - lonDeltaTotal, center.lon - lonDeltaTotal + stepLon .. center.lon + lonDeltaTotal]

-- | Approximate cell width in metres for a given geohash precision.
-- Based on standard geohash cell dimensions.
geohashCellWidthMeters :: Int -> Double
geohashCellWidthMeters precision = case precision of
  1 -> 5000000.0
  2 -> 1250000.0
  3 -> 156000.0
  4 -> 39100.0
  5 -> 4890.0
  6 -> 1220.0
  7 -> 153.0
  8 -> 38.0
  _ -> 1220.0 -- default to precision 6
