-- | Pure helpers for the OTP plan-response Redis cache.
--
-- The cache is keyed on a geohash-precision-7 normalization of origin/destination
-- (~150m cells), a hash of the routing-relevant request params, and a feed-version
-- token (supplied by the caller) so a GTFS/graph rebuild auto-invalidates entries.
--
-- Stored value = base64(gzip(json)) of the 'MultiModalResponse' with the
-- verified-unused fields ('polyline', 'serviceTypes') blanked AND the two
-- user-PII walk-leg endpoints neutralized:
--   * first leg's startLocation (+ its routeDetails[].startLocation) if Walk
--   * last leg's endLocation   (+ its routeDetails[].endLocation)   if Walk
-- so a cache-filler's exact origin/destination can never leak to a nearby user
-- who hits the same geohash cell. On read the plan's synthetic timestamps are
-- re-anchored to the current query time and those two walk endpoints are
-- overwritten with the requesting user's own origin/destination
-- (see 'neutralizeForCache' / 'reanchorTimes' / 'restoreWalkEndpoints').
module SharedLogic.MultiModal.PlanCache
  ( geohashP7,
    planCacheKeyPrefix,
    mkPlanCacheKey,
    encodeForRedis,
    decodeFromRedis,
    blankForCache,
    reanchorTimes,
    getTransitRoutesCached,
  )
where

import qualified BecknV2.FRFS.Utils as GzipUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Crypto.Hash as Hash
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Geohash as DG
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, diffUTCTime)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderConfig as DRC
import qualified Environment
import qualified Kernel.External.Maps.Google.MapsClient.Types as GT
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Common (MonadFlow, getCurrentTime)
import Kernel.Types.Id (Id)
import Kernel.Types.TryException (withTryCatch)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Tools.Metrics.BAPMetrics as BAPMetrics

-- --------------------------------------------------------------------------
-- Geohash precision-7 (~150m cells), via the same 'Data.Geohash' library the
-- rest of the codebase uses. 'geohashP7' stays total so it can be used in the
-- cache key and cell-center computation without threading Maybe; a geohash of a
-- valid lat/lon never fails, so falling back to "" is unreachable in practice.
-- --------------------------------------------------------------------------

geohashP7 :: Double -> Double -> Text
geohashP7 lat lon = T.pack (fromMaybe "" (DG.encode 7 (lat, lon)))

-- --------------------------------------------------------------------------
-- Cache key: otp:plan:v1:{verHash}:{gh7From}:{gh7To}:{paramHash}
-- verHash and paramHash are short hex so per-key overhead stays bounded.
-- --------------------------------------------------------------------------

planCacheKeyPrefix :: Text
planCacheKeyPrefix = "otp:plan:v1"

mkPlanCacheKey :: Text -> GetTransitRoutesReq -> Text
mkPlanCacheKey verHash req =
  T.intercalate
    ":"
    [ planCacheKeyPrefix,
      verHash,
      geohashP7 req.origin.location.latLng.latitude req.origin.location.latLng.longitude,
      geohashP7 req.destination.location.latLng.latitude req.destination.location.latLng.longitude,
      paramHash req
    ]

-- | Hash of every request field that actually changes OTP's output, EXCLUDING
-- origin/destination (captured by the geohashes) and the volatile timestamps
-- (arrivalTime/departureTime) which are safe to strip for frequency-based feeds.
paramHash :: GetTransitRoutesReq -> Text
paramHash req = shortHashHex . BSL.toStrict $ A.encode canonical
  where
    -- Ordered JSON array (not an object) so encoding is order-deterministic
    -- across processes/aeson versions, giving a stable hash for identical requests.
    canonical :: [A.Value]
    canonical =
      [ A.toJSON (L.sort (map show req.permissibleModes) :: [Text]),
        A.toJSON (show req.minimumWalkDistance :: String),
        A.toJSON req.maxAllowedPublicTransportLegs,
        A.toJSON (show req.sortingType :: String),
        A.toJSON (fmap roundTo1 req.walkSpeed),
        A.toJSON req.mode,
        A.toJSON req.transitPreferences,
        A.toJSON req.transportModes
      ]
    roundTo1 x = fromIntegral (round (x * 10) :: Integer) / 10 :: Double

shortHashHex :: BS.ByteString -> Text
shortHashHex bs = T.take 12 . T.pack $ show (Hash.hashWith Hash.SHA256 bs)

-- --------------------------------------------------------------------------
-- Redis codec: base64(gzip(json(blanked response))). base64 keeps the blob
-- storable through the JSON-typed Hedis setExp/get without a raw-bytes path.
-- --------------------------------------------------------------------------

encodeForRedis :: GetTransitRoutesReq -> MultiModalResponse -> Text
encodeForRedis req =
  TE.decodeUtf8 . B64.encode . GzipUtils.compressGzipBody . BSL.toStrict . A.encode . blankForCache req

-- | Returns Nothing on any base64/gzip/JSON failure so the caller treats a
-- corrupt or schema-changed entry as a cache miss rather than crashing.
decodeFromRedis :: (MonadFlow m) => Text -> m (Maybe MultiModalResponse)
decodeFromRedis txt =
  case B64.decode (TE.encodeUtf8 txt) of
    Left _ -> pure Nothing
    Right gz -> do
      raw <- GzipUtils.decompressGzipBody gz
      pure $ A.decode (BSL.fromStrict raw)

-- --------------------------------------------------------------------------
-- Field trimming + PII neutralization applied before storing.
--
--   * polyline   : never read off MultiModalLeg (dropped at the Leg boundary)
--   * serviceTypes: 0 reads
--   * first walk leg startLocation / last walk leg endLocation : the
--     requester's exact origin/destination. These are PII and must not be
--     written to Redis (a later hit from a contiguous geohash cell would
--     render the cache-filler's home/destination to another user). We replace
--     them with the geohash-cell center of the same request so the value is
--     still a plausible coordinate, never the user's own.
--
-- Calling with no Walk legs (or a walk leg that is not first/last) is a no-op,
-- so serviceability / transit-only plans are untouched.
-- --------------------------------------------------------------------------

blankForCache :: GetTransitRoutesReq -> MultiModalResponse -> MultiModalResponse
blankForCache req = blankUnused . neutralizeFirstLast req
  where
    blankUnused resp = resp {routes = map blankRoute resp.routes}
    blankRoute r = r {legs = map blankLeg r.legs}
    blankLeg l = l {polyline = GT.Polyline {GT.encodedPolyline = ""}, serviceTypes = []}

-- | Rewrite the first Walk leg's start and the last Walk leg's end to the given
-- coordinates (leg-level and their routeDetails). Legs in the middle and any
-- non-first/last Walk legs are untouched, so transit-only plans are unchanged.
setWalkEndpoints :: GT.LocationV2 -> GT.LocationV2 -> MultiModalRoute -> MultiModalRoute
setWalkEndpoints fromLoc toLoc r = r {legs = zipWith idxLeg [0 ..] r.legs}
  where
    n = length r.legs
    -- Apply start and end independently (not first-match-wins) so a single-leg
    -- Walk route (i == 0 and i == n - 1) gets BOTH endpoints rewritten, not just
    -- the start. Multi-leg routes keep the first-only / last-only behavior.
    idxLeg i l =
      let l' = if i == 0 && l.mode == Walk then setStart fromLoc l else l
       in if i == n - 1 && l'.mode == Walk then setEnd toLoc l' else l'

-- | Replace the first walk leg's start / last walk leg's end with the geohash
-- cell centers of the request's origin/destination, so no user coordinates are
-- ever written to Redis. A later hit from a contiguous cell would otherwise
-- render the cache-filler's home/destination to another user.
neutralizeFirstLast :: GetTransitRoutesReq -> MultiModalResponse -> MultiModalResponse
neutralizeFirstLast req resp = resp {routes = map (setWalkEndpoints fromCell toCell) resp.routes}
  where
    hashOf wp = geohashP7 wp.location.latLng.latitude wp.location.latLng.longitude
    fromCell = GT.LocationV2 {GT.latLng = geohashCellCenter (hashOf req.origin)}
    toCell = GT.LocationV2 {GT.latLng = geohashCellCenter (hashOf req.destination)}

-- | Overwrite the first walk leg's start / last walk leg's end with the
-- requesting user's own origin/destination, undoing the neutralization done at
-- store time. Runs on both hit and miss so the two paths return identical
-- output (as the module already guarantees for re-anchoring).
restoreWalkEndpoints :: GetTransitRoutesReq -> MultiModalResponse -> MultiModalResponse
restoreWalkEndpoints req resp =
  resp {routes = map (setWalkEndpoints req.origin.location req.destination.location) resp.routes}

-- | Cell-center coordinates of a geohash cell (non-PII substitute for a
-- neutralized walk endpoint). 'Data.Geohash.decode_' decodes the geohash back
-- to its bounding box ((latmin,latmax),(lonmin,lonmax)); we take the midpoint.
-- Defaults to (0,0) if the geohash string is somehow invalid (never expected,
-- we only feed it strings we produced ourselves).
geohashCellCenter :: Text -> GT.LatLngV2
geohashCellCenter gh =
  case DG.decode_ (T.unpack gh) of
    Nothing -> GT.LatLngV2 {GT.latitude = 0, GT.longitude = 0}
    Just ((latLo, latHi), (lonLo, lonHi)) ->
      GT.LatLngV2 {GT.latitude = (latLo + latHi) / 2, GT.longitude = (lonLo + lonHi) / 2}

-- | Set a leg's start coordinate (leg-level + its routeDetails[].startLocation),
-- preserving all other fields.
setStart :: GT.LocationV2 -> MultiModalLeg -> MultiModalLeg
setStart loc l =
  l
    { startLocation = loc,
      routeDetails = map (setRdStart loc) l.routeDetails
    }
  where
    setRdStart :: GT.LocationV2 -> MultiModalRouteDetails -> MultiModalRouteDetails
    setRdStart rdLoc rd = rd {startLocation = rdLoc}

-- | Set a leg's end coordinate (leg-level + its routeDetails[].endLocation).
setEnd :: GT.LocationV2 -> MultiModalLeg -> MultiModalLeg
setEnd loc l =
  l
    { endLocation = loc,
      routeDetails = map (setRdEnd loc) l.routeDetails
    }
  where
    setRdEnd :: GT.LocationV2 -> MultiModalRouteDetails -> MultiModalRouteDetails
    setRdEnd rdLoc rd = rd {endLocation = rdLoc}

-- --------------------------------------------------------------------------
-- Re-anchoring: OTP's absolute times are synthetic (departureTime + cumulative
-- durations) for frequency-based feeds. On a hit we shift every timestamp by a
-- single delta so it matches the current query time while preserving all
-- inter-leg spacing. Anchor = the earliest timestamp present in the plan.
-- --------------------------------------------------------------------------

reanchorTimes :: UTCTime -> MultiModalResponse -> MultiModalResponse
reanchorTimes newAnchor resp =
  case allTimes resp of
    [] -> resp
    ts -> shiftResp (diffUTCTime newAnchor (minimum ts)) resp

allTimes :: MultiModalResponse -> [UTCTime]
allTimes resp = concatMap routeTimes resp.routes
  where
    routeTimes r = catMaybes [r.startTime, r.endTime] ++ concatMap legTimes r.legs
    legTimes l =
      catMaybes [l.fromArrivalTime, l.fromDepartureTime, l.toArrivalTime, l.toDepartureTime]
        ++ concatMap rdTimes l.routeDetails
    rdTimes rd = catMaybes [rd.fromArrivalTime, rd.fromDepartureTime, rd.toArrivalTime, rd.toDepartureTime]

shiftResp :: NominalDiffTime -> MultiModalResponse -> MultiModalResponse
shiftResp delta resp = resp {routes = map shiftRoute resp.routes}
  where
    s = fmap (addUTCTime delta)
    shiftRoute r = r {startTime = s r.startTime, endTime = s r.endTime, legs = map shiftLeg r.legs}
    shiftLeg l =
      l
        { fromArrivalTime = s l.fromArrivalTime,
          fromDepartureTime = s l.fromDepartureTime,
          toArrivalTime = s l.toArrivalTime,
          toDepartureTime = s l.toDepartureTime,
          routeDetails = map shiftRd l.routeDetails
        }
    shiftRd :: MultiModalRouteDetails -> MultiModalRouteDetails
    shiftRd rd =
      rd
        { fromArrivalTime = s rd.fromArrivalTime,
          fromDepartureTime = s rd.fromDepartureTime,
          toArrivalTime = s rd.toArrivalTime,
          toDepartureTime = s rd.toDepartureTime
        }

-- --------------------------------------------------------------------------
-- Cache-aside wrapper. Lives here (not Tools.MultiModal) because it needs
-- OTPRest.getGtfsVersion, and OTPRest imports Tools.MultiModal -> would cycle.
-- --------------------------------------------------------------------------

-- | Backstop TTL used when the cache is enabled but no per-city TTL is set.
defaultPlanCacheTtlSeconds :: Int
defaultPlanCacheTtlSeconds = 172800 -- 2 days

-- | Feed-version token: hashed "#"-join of every city feed's GTFS version plus
-- the rider config's public-transport data version. A feed/graph rebuild changes
-- a GTFS version, changing this token, which auto-invalidates all keys.
-- Memoized ~60s per city so we don't OTP-round-trip on every plan lookup.
getPlanCacheVersion :: Id DMOC.MerchantOperatingCity -> Maybe DRC.RiderConfig -> Environment.Flow Text
getPlanCacheVersion mocId mbRiderConfig =
  IM.withInMemCache ["otpPlanCacheVer", mocId.getId] 60 $ do
    integratedBPPConfigs <-
      concat
        <$> mapM
          (\vType -> SIBC.findAllIntegratedBPPConfig mocId vType DIBC.MULTIMODAL)
          [Enums.BUS, Enums.METRO, Enums.SUBWAY]
    -- Bind each version to its feedKey ("feedKey:version") and sort, so the token
    -- is a faithful, order-independent fingerprint of the {feed -> version} map.
    -- Sorting bare versions would lose the feed binding (two feeds swapping
    -- content would collide); sorting the pair keeps it. This also makes the
    -- token stable across pods when a mode has multiple configs (unordered list).
    versioned <-
      withTryCatch "otpPlanCache:getGtfsVersion" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
        Left _ -> pure (map (.feedKey) integratedBPPConfigs)
        Right vs -> pure (zipWith (\c v -> c.feedKey <> ":" <> v) integratedBPPConfigs vs)
    let raw = T.intercalate "#" (L.sort versioned) <> maybe "" (\v -> "#" <> show v) (mbRiderConfig >>= (.domainPublicTransportDataVersion))
    pure $ shortHashHex (TE.encodeUtf8 raw)

-- | Cache-aside in front of 'MultiModal.getTransitRoutes'. When the per-city
-- flag is off it is a transparent passthrough. On a hit it decodes and
-- re-anchors; on a miss it stores the blanked response and returns the same
-- blanked+re-anchored value so hit/miss output is identical. Redis failures are
-- swallowed by the Hedis layer (get -> Nothing, set -> logged), so the cache can
-- never fail a request.
getTransitRoutesCached ::
  Id DMOC.MerchantOperatingCity ->
  Id DMerchant.Merchant ->
  Maybe Text ->
  MultiModalServiceConfig ->
  GetTransitRoutesReq ->
  Environment.Flow (Maybe MultiModalResponse)
getTransitRoutesCached mocId merchantId mbReqId serviceReq req = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing
  let enabled = fromMaybe False (mbRiderConfig >>= (.otpPlanCacheEnabled))
  if not enabled
    then callOtp
    else do
      merchantShortId <- resolveMerchantShortId
      verHash <- getPlanCacheVersion mocId mbRiderConfig
      now <- getCurrentTime
      let key = mkPlanCacheKey verHash req
          anchor = fromMaybe now req.departureTime
          -- A configured TTL <= 0 would make Redis EXPIRE drop the key immediately,
          -- silently disabling the cache; fall back to the default in that case.
          ttl = case mbRiderConfig >>= (.otpPlanCacheTtlSeconds) of
            Just t | t > 0 -> t
            _ -> defaultPlanCacheTtlSeconds
      let serve cached =
            reanchorTimes anchor (restoreWalkEndpoints req cached)
      Hedis.get key >>= \case
        Just txt ->
          decodeFromRedis txt >>= \case
            Just resp -> do
              BAPMetrics.incrementOtpPlanCacheCounter "hit" merchantShortId mocId.getId
              pure (Just (serve resp))
            Nothing -> onMiss "error" merchantShortId key anchor ttl
        Nothing -> onMiss "miss" merchantShortId key anchor ttl
  where
    callOtp = MultiModal.getTransitRoutes mbReqId serviceReq req
    -- Merchant shortId for the metric label (single in-mem-cached lookup;
    -- merchantId is passed in by the caller, so no MOC->merchant hop needed).
    -- Falls back to "UNKNOWN" (not the MOC id, a different entity) if the
    -- merchant row can't be resolved, keeping the label merchant-shaped.
    resolveMerchantShortId = do
      mbMerchant <- CQM.findById merchantId
      pure $ maybe "UNKNOWN" (\m -> m.shortId.getShortId) mbMerchant
    onMiss result merchantShortId key anchor ttl = do
      BAPMetrics.incrementOtpPlanCacheCounter result merchantShortId mocId.getId
      callOtp >>= \case
        Just resp -> do
          Hedis.setExp key (encodeForRedis req resp) ttl
          pure (Just (reanchorTimes anchor (restoreWalkEndpoints req (blankForCache req resp))))
        Nothing -> pure Nothing
