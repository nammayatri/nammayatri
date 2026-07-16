-- | Pure helpers for the OTP plan-response Redis cache.
--
-- The cache is keyed on a geohash-precision-7 normalization of origin/destination
-- (~150m cells), a hash of the routing-relevant request params, and a feed-version
-- token (supplied by the caller) so a GTFS/graph rebuild auto-invalidates entries.
--
-- Stored value = base64(gzip(json)) of the 'MultiModalResponse' with the two
-- verified-unused fields ('polyline', 'serviceTypes') blanked. On read the plan's
-- synthetic timestamps are re-anchored to the current query time (see 'reanchorTimes').
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
import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, diffUTCTime)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
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
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Tools.Metrics.BAPMetrics as BAPMetrics

-- --------------------------------------------------------------------------
-- Geohash (precision 7 ~= 150m cells). Standard base32 interleaving of
-- longitude (even bits) and latitude (odd bits). Validated against the
-- reference vectors "ezs42" (42.6,-5.6 p5) and "u4pruyd" (57.64911,10.40744 p7).
-- --------------------------------------------------------------------------

base32Chars :: String
base32Chars = "0123456789bcdefghjkmnpqrstuvwxyz"

geohashP7 :: Double -> Double -> Text
geohashP7 = encodeGeohash 7

encodeGeohash :: Int -> Double -> Double -> Text
encodeGeohash prec lat lon =
  T.pack $ map toChar $ Split.chunksOf 5 $ take (prec * 5) (genBits True (-180, 180) (-90, 90))
  where
    genBits :: Bool -> (Double, Double) -> (Double, Double) -> [Int]
    genBits isEven lonR@(lonLo, lonHi) latR@(latLo, latHi)
      | isEven =
        let mid = (lonLo + lonHi) / 2
         in if lon >= mid
              then 1 : genBits False (mid, lonHi) latR
              else 0 : genBits False (lonLo, mid) latR
      | otherwise =
        let mid = (latLo + latHi) / 2
         in if lat >= mid
              then 1 : genBits True lonR (mid, latHi)
              else 0 : genBits True lonR (latLo, mid)
    toChar bits5 = base32Chars !! foldl' (\acc b -> acc * 2 + b) 0 bits5

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
    canonical =
      A.object
        [ "modes" A..= (L.sort (map show req.permissibleModes) :: [Text]),
          "walkDist" A..= (show req.minimumWalkDistance :: String),
          "maxLegs" A..= req.maxAllowedPublicTransportLegs,
          "sort" A..= (show req.sortingType :: String),
          "walkSpeed" A..= fmap roundTo1 req.walkSpeed,
          "mode" A..= req.mode,
          "transitPrefs" A..= req.transitPreferences,
          "transportModes" A..= req.transportModes
        ]
    roundTo1 x = fromIntegral (round (x * 10) :: Integer) / 10 :: Double

shortHashHex :: BS.ByteString -> Text
shortHashHex bs = T.take 12 . T.pack $ show (Hash.hashWith Hash.SHA256 bs)

-- --------------------------------------------------------------------------
-- Redis codec: base64(gzip(json(blanked response))). base64 keeps the blob
-- storable through the JSON-typed Hedis setExp/get without a raw-bytes path.
-- --------------------------------------------------------------------------

encodeForRedis :: MultiModalResponse -> Text
encodeForRedis =
  TE.decodeUtf8 . B64.encode . GzipUtils.compressGzipBody . BSL.toStrict . A.encode . blankForCache

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
-- Field trimming: polyline (never read off MultiModalLeg; dropped at the
-- MultiModalLeg -> JourneyLeg boundary) and serviceTypes (0 reads).
-- --------------------------------------------------------------------------

blankForCache :: MultiModalResponse -> MultiModalResponse
blankForCache resp = resp {routes = map blankRoute resp.routes}
  where
    blankRoute r = r {legs = map blankLeg r.legs}
    blankLeg l = l {polyline = GT.Polyline {GT.encodedPolyline = ""}, serviceTypes = []}

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
    gtfsVersions <-
      withTryCatch "otpPlanCache:getGtfsVersion" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
        Left _ -> pure (map (.feedKey) integratedBPPConfigs)
        Right vs -> pure vs
    let raw = T.intercalate "#" gtfsVersions <> maybe "" (\v -> "#" <> show v) (mbRiderConfig >>= (.domainPublicTransportDataVersion))
    pure $ shortHashHex (TE.encodeUtf8 raw)

-- | Cache-aside in front of 'MultiModal.getTransitRoutes'. When the per-city
-- flag is off it is a transparent passthrough. On a hit it decodes and
-- re-anchors; on a miss it stores the blanked response and returns the same
-- blanked+re-anchored value so hit/miss output is identical. Redis failures are
-- swallowed by the Hedis layer (get -> Nothing, set -> logged), so the cache can
-- never fail a request.
getTransitRoutesCached ::
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  MultiModalServiceConfig ->
  GetTransitRoutesReq ->
  Environment.Flow (Maybe MultiModalResponse)
getTransitRoutesCached mocId mbReqId serviceReq req = do
  mbRiderConfig <- CQRC.findByMerchantOperatingCityId mocId
  let enabled = maybe False (fromMaybe False . (.otpPlanCacheEnabled)) mbRiderConfig
  if not enabled
    then callOtp
    else do
      verHash <- getPlanCacheVersion mocId mbRiderConfig
      now <- getCurrentTime
      let key = mkPlanCacheKey verHash req
          anchor = fromMaybe now req.departureTime
          ttl = fromMaybe defaultPlanCacheTtlSeconds (mbRiderConfig >>= (.otpPlanCacheTtlSeconds))
      Hedis.get key >>= \case
        Just txt ->
          decodeFromRedis txt >>= \case
            Just resp -> do
              BAPMetrics.incrementOtpPlanCacheCounter "hit" mocId.getId
              pure (Just (reanchorTimes anchor resp))
            Nothing -> onMiss "error" key anchor ttl
        Nothing -> onMiss "miss" key anchor ttl
  where
    callOtp = MultiModal.getTransitRoutes mbReqId serviceReq req
    onMiss result key anchor ttl = do
      BAPMetrics.incrementOtpPlanCacheCounter result mocId.getId
      callOtp >>= \case
        Just resp -> do
          Hedis.setExp key (encodeForRedis resp) ttl
          pure (Just (reanchorTimes anchor (blankForCache resp)))
        Nothing -> pure Nothing
