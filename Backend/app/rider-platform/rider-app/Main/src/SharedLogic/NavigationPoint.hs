{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Caches Google Destinations v4 @navigationPoints@ that are drivable, so that
-- clients which don't call v4 themselves (Android) can still be offered pickup
-- points that sit on a road.
--
-- Points are written whenever any client calls @/maps/searchDestinations@ and read
-- back by the serviceability API. This is separate from hotspots: nothing here
-- touches the hotspot keys, counters or 'HotSpotConfig'. Behaviour is driven by the
-- per-city 'NavigationPointConfig'.
--
-- Storage is one Redis hash per geohash cell at 'bucketGeohashPrecision' (7 ≈ 150 m):
--
-- > NavPoint:<merchantId>:<bucketGeohash>   field <pointGeohash>   value CachedNavPoint
--
-- The hash TTL is reset on every write. Each point also carries 'fetchedAt', and
-- points older than the TTL are skipped and deleted on read, so a point never
-- outlives the TTL from the last time Google returned it, however busy its cell is.
-- Changing either precision leaves points written under the old one unread; they
-- expire on their own within the TTL.
module SharedLogic.NavigationPoint
  ( findEnabledNavPointConfig,
    withRequiredNavPointFields,
    cacheNavPoints,
    getNearbyNavPoints,
  )
where

import qualified Data.Geohash as DG
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Domain.Types.HotSpot as DHotSpot
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.NavigationPointConfig as DNPC
import qualified Kernel.External.Maps.Google.MapsClient.Types as GMaps
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.NavigationPointConfig (NavigationPointConfigDimensions (..))
import qualified Tools.Maps as Maps

data CachedNavPoint = CachedNavPoint
  { lat :: Double,
    lon :: Double,
    placeId :: Maybe Text,
    fetchedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- | Google Maps Platform terms allow caching lat/long for at most 30 days, so the
-- configured TTL is capped here.
maxNavPointTtl :: Int
maxNavPointTtl = 30 * 24 * 60 * 60

-- | Guards against a misconfigured radius turning one read into hundreds of Redis calls.
maxNavPointRadius :: Double
maxNavPointRadius = 1000

-- | Upper bound on the geohash buckets (one Redis read each) a single origin lookup
-- may touch. Radius alone does not bound this: a fine bucket precision turns even a
-- small radius into thousands of cells.
maxNavPointBuckets :: Int
maxNavPointBuckets = 100

-- | Geohash supports precisions 1 to 12.
clampPrecision :: Int -> Int
clampPrecision = max 1 . min 12

navPointKey :: Id DMerchant.Merchant -> Text -> Text
navPointKey merchantId bucket = "NavPoint:" <> merchantId.getId <> ":" <> bucket

-- | The city's 'NavigationPointConfig' when it exists and is enabled. A failed
-- lookup counts as disabled, so callers never fail because of it.
findEnabledNavPointConfig :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m (Maybe DNPC.NavigationPointConfig)
findEnabledNavPointConfig merchantOperatingCityId = do
  res <- withTryCatch "findEnabledNavPointConfig" $ getConfig (NavigationPointConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) Nothing
  case res of
    Left err -> do
      logError $ "Failed to load NavigationPointConfig: " <> show err
      pure Nothing
    Right mbCfg -> pure $ mfilter (.enabled) mbCfg

ttlSeconds :: DNPC.NavigationPointConfig -> Int
ttlSeconds cfg = max 1 $ min maxNavPointTtl cfg.ttlInSeconds

bucketPrecision :: DNPC.NavigationPointConfig -> Int
bucketPrecision cfg = clampPrecision cfg.bucketGeohashPrecision

-- | Never coarser than the bucket, so a point always falls inside exactly one bucket.
pointPrecision :: DNPC.NavigationPointConfig -> Int
pointPrecision cfg = max (bucketPrecision cfg) (clampPrecision cfg.pointGeohashPrecision)

-- | Adds the fields of the configured 'requiredFieldMask' (comma-separated, Google's
-- format, e.g. @destinations.navigationPoints,destinations.primary.place@) that the
-- client's mask lacks. Unset or blank (the default) sends the client's mask as-is,
-- since widening it can change Google billing; points are then cached only when the
-- client asked for them. A missing client mask and @*@ already mean "all fields" and
-- are left alone. A field covered by a parent path (e.g. @destinations@) is not added.
withRequiredNavPointFields :: DNPC.NavigationPointConfig -> Maps.SearchDestinationsReq -> Maps.SearchDestinationsReq
withRequiredNavPointFields cfg req@Maps.SearchDestinationsReq {..}
  | null required = req
  | otherwise = Maps.SearchDestinationsReq {fieldMask = mergeFieldMask <$> fieldMask, ..}
  where
    splitMask = filter (not . T.null) . map T.strip . T.splitOn ","
    required = maybe [] splitMask cfg.requiredFieldMask
    mergeFieldMask clientMask =
      let fields = splitMask clientMask
          isCovered r = any (\f -> f == r || (f <> ".") `T.isPrefixOf` r) fields
          missing = DL.nub $ filter (not . isCovered) required
       in if null fields || "*" `elem` fields || null missing
            then clientMask
            else T.intercalate "," (fields <> missing)

-- | Stores every drivable navigation point in the response. Errors are logged and
-- swallowed: caching must never affect the v4 response the client gets.
cacheNavPoints ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DNPC.NavigationPointConfig ->
  Maps.SearchDestinationsResp ->
  m ()
cacheNavPoints cfg resp = do
  now <- getCurrentTime
  let points = DL.nubBy (\a b -> fst a == fst b) $ concatMap (destinationPoints now) (fromMaybe [] resp.destinations)
  res <- withTryCatch "cacheNavPoints" $
    forM_ points $ \(geohash, point) ->
      Hedis.hSetExp (navPointKey cfg.merchantId (T.take (bucketPrecision cfg) geohash)) geohash point (ttlSeconds cfg)
  case res of
    Left err -> logError $ "Failed to cache navigation points: " <> show err
    Right () -> logDebug $ "Cached " <> show (length points) <> " navigation points"
  where
    destinationPoints :: UTCTime -> GMaps.Destination -> [(Text, CachedNavPoint)]
    destinationPoints now destination =
      let mbPlaceId = stripPlacesPrefix <$> (destination.primary >>= (.place))
       in mapMaybe (toCachedPoint now mbPlaceId) (fromMaybe [] destination.navigationPoints)

    toCachedPoint :: UTCTime -> Maybe Text -> GMaps.NavigationPoint -> Maybe (Text, CachedNavPoint)
    toCachedPoint now mbPlaceId navPoint = do
      guard $ maybe False ("DRIVE" `elem`) navPoint.travelModes
      location <- navPoint.location
      geohash <- DG.encode (pointPrecision cfg) (location.latitude, location.longitude)
      pure (T.pack geohash, CachedNavPoint {lat = location.latitude, lon = location.longitude, placeId = mbPlaceId, fetchedAt = now})

    stripPlacesPrefix place = fromMaybe place (T.stripPrefix "places/" place)

-- | Cached navigation points within the configured radius of the location, nearest
-- first. Returns an empty list on any Redis error so serviceability never fails here.
getNearbyNavPoints ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DNPC.NavigationPointConfig ->
  LatLong ->
  m [DHotSpot.HotSpotInfo]
getNearbyNavPoints cfg origin = do
  now <- getCurrentTime
  let radius = min maxNavPointRadius $ max 0 cfg.radiusInMeters
  case nearbyBuckets (bucketPrecision cfg) radius origin of
    Nothing -> do
      logError $
        "NavigationPointConfig for " <> cfg.merchantOperatingCityId.getId <> " needs more than " <> show maxNavPointBuckets
          <> " geohash buckets per lookup (radiusInMeters "
          <> show radius
          <> ", bucketGeohashPrecision "
          <> show (bucketPrecision cfg)
          <> "); skipping navigation points"
      pure []
    Just buckets -> readBuckets now radius buckets
  where
    readBuckets now radius buckets = do
      res <- withTryCatch "getNearbyNavPoints" $ concat <$> mapM (liveBucketPoints now) buckets
      case res of
        Left err -> do
          logError $ "Failed to read navigation points: " <> show err
          pure []
        Right points ->
          pure
            . map (toHotSpotInfo . snd)
            . take cfg.maxPointsToShow
            . DL.sortOn fst
            $ mapMaybe (withinRadius radius) points

    liveBucketPoints now bucket = do
      let key = navPointKey cfg.merchantId bucket
      entries :: [(Text, CachedNavPoint)] <- Hedis.hGetAll key
      let (expired, live) = DL.partition (\(_, point) -> DT.diffUTCTime now point.fetchedAt > fromIntegral (ttlSeconds cfg)) entries
      Hedis.hDel key (map fst expired)
      pure live

    withinRadius radius entry@(_, point) =
      let distance = getMeters . highPrecMetersToMeters $ distanceBetweenInMeters origin (LatLong point.lat point.lon)
       in if fromIntegral distance <= radius then Just (distance, entry) else Nothing

    toHotSpotInfo (geohash, point) =
      DHotSpot.HotSpotInfo
        { _geoHash = geohash,
          _centroidLatLong = LatLong point.lat point.lon,
          _address = Nothing
        }

-- | Geohash cells covering the square around the location with half-side @radius@.
-- Samples are one cell apart, so every cell touching the square gets one.
-- 'Nothing' when that would exceed 'maxNavPointBuckets'; the count is estimated
-- before any sample is generated.
nearbyBuckets :: Int -> Double -> LatLong -> Maybe [Text]
nearbyBuckets precision radius center
  | estimatedCells > maxNavPointBuckets = Nothing
  | otherwise = Just . DL.nub . map T.pack $ mapMaybe (\(sampleLat, sampleLon) -> DG.encode precision (sampleLat, sampleLon)) samples
  where
    -- Upper bound on the cells along each axis: the span plus a partial cell at both ends.
    cellsAlong span' cellSpan = ceiling (2 * span' / cellSpan) + 2 :: Int
    estimatedCells = cellsAlong radiusLat latSpan * cellsAlong radiusLon lonSpan
    bits = 5 * precision
    latSpan = 180 / (2 :: Double) ^ (bits `div` 2)
    lonSpan = 360 / (2 :: Double) ^ (bits - bits `div` 2)
    metersPerDegreeLat = 111320 :: Double
    metersPerDegreeLon = max 1 (metersPerDegreeLat * cos (center.lat * pi / 180))
    radiusLat = radius / metersPerDegreeLat
    radiusLon = radius / metersPerDegreeLon
    steps lo hi cellSpan = takeWhile (< hi) [lo, lo + cellSpan ..] <> [hi]
    samples =
      [ (sampleLat, sampleLon)
        | sampleLat <- steps (center.lat - radiusLat) (center.lat + radiusLat) latSpan,
          sampleLon <- steps (center.lon - radiusLon) (center.lon + radiusLon) lonSpan
      ]
