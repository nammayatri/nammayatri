module Domain.Action.UI.DemandHotspots
  ( getDriverDemandHotspots,
    postDriverDemandHotspot,
    tripOptionFromCategories,
    updateDemandHotspotsOnSearch,
    removeDemandHotspot,
    getActiveSearchLocations,
  )
where

import API.Types.UI.DemandHotspots
import Data.Aeson (withArray)
import qualified Data.Aeson as Ae
import qualified Data.Geohash as Geohash
import Data.List (groupBy, nub)
import qualified Data.List.NonEmpty as NE
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Time as T
import qualified Data.Vector as V
import Domain.Types.Extra.LeanFlow (LeanFlowFeature (DEMAND_HOTSPOTS))
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.SearchRequest
import Domain.Types.TransporterConfig
import qualified Domain.Types.Trip as Trip
import Environment
import EulerHS.Prelude hiding (foldr', groupBy, id, length, map, mapM_, null, sum, whenJust)
import GHC.Num.Integer (integerToInt)
import Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Storage.Hedis.Error
import Kernel.Types.APISuccess
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Time (utcToMilliseconds)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.NudgeRide as NudgeRide
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.CachedQueries.SystemConfigs.LeanFlow as CQLF
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVeh
import qualified Tools.Maps as TMaps

-- What it is:
-- Entire city is divided into geohashes. A search adds a member and a ride start
-- removes that same member, so hotspots depict searches that have not started.
--
-- Redis data structures we are using - set and sorted set
-- Set - For storing all geohashes we have in a city
-- Sorted Set - Storing searches categorised by cityId and geohash
--
-- On Search - We add the search to the sorted set of that city and geohash
-- On Ride start - We remove that search. Booking no longer removes it.
--
-- Member encoding:
--   new writes: ["id6", lat, lon, product, isScheduled]
--   older writes still in Redis: ["id6", lat, lon]
-- Removal tries exact 5- then 3-element zRem; only if both miss, scan by search-id prefix.
--
-- Dummy member written before product was stored. Same geohash key as OneWay and Rental.
-- There is no product, so it cannot be split. It decodes as product Nothing, isScheduled False,
-- and the hotspot category is tripOption "Unknown".
--   ["a1b2c3", 12.9716, 77.5946]

data HotspotObject = HotspotObject
  { hotspotSearchId :: Text,
    hotspotLat :: Double,
    hotspotLon :: Double,
    hotspotProduct :: Maybe Text,
    hotspotIsScheduled :: Bool
  }
  deriving (Show)

instance ToJSON HotspotObject where
  toJSON HotspotObject {..} =
    toJSON [toJSON hotspotSearchId, toJSON hotspotLat, toJSON hotspotLon, toJSON (fromMaybe "Unknown" hotspotProduct), toJSON hotspotIsScheduled]

-- Same bytes as members written before product and isScheduled were stored.
newtype LegacyHotspotObject = LegacyHotspotObject HotspotObject

instance ToJSON LegacyHotspotObject where
  toJSON (LegacyHotspotObject HotspotObject {..}) =
    toJSON [toJSON hotspotSearchId, toJSON hotspotLat, toJSON hotspotLon]

instance FromJSON HotspotObject where
  parseJSON = withArray "HotspotObject" $ \vec ->
    case V.length vec of
      3 ->
        HotspotObject
          <$> parseJSON (vec V.! 0)
          <*> parseJSON (vec V.! 1)
          <*> parseJSON (vec V.! 2)
          <*> pure Nothing
          <*> pure False
      5 ->
        HotspotObject
          <$> parseJSON (vec V.! 0)
          <*> parseJSON (vec V.! 1)
          <*> parseJSON (vec V.! 2)
          <*> (Just <$> parseJSON (vec V.! 3))
          <*> parseJSON (vec V.! 4)
      _ -> fail "Expected an array of three or five elements"

-- One search is one product. Offer modes (RideOtp vs DynamicOffer) are not stored.
-- Named apart from HotspotObject.hotspotProduct so it does not clash with the field selector.
tripOptionFromCategories :: [Trip.TripCategory] -> Text
tripOptionFromCategories categories = case categories of
  Trip.OneWay Trip.MeterRide : _ -> "MeterRide"
  Trip.OneWay _ : _ -> "OneWay"
  Trip.Rental _ : _ -> "Rental"
  Trip.RideShare _ : _ -> "RideShare"
  Trip.InterCity _ _ : _ -> "InterCity"
  Trip.CrossCity _ _ : _ -> "CrossCity"
  Trip.Ambulance _ : _ -> "Ambulance"
  Trip.Delivery _ : _ -> "Delivery"
  Trip.EasyBooking _ : _ -> "EasyBooking"
  Trip.IntercityRental _ _ : _ -> "IntercityRental"
  Trip.SharedRoute _ : _ -> "SharedRoute"
  [] -> "Unknown"

getDriverDemandHotspots ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow GetDemandHotspotsResp
  )
getDriverDemandHotspots (_, _, merchantOpCityId) = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  demandHotspotsExcluded <- CQLF.isFeatureExcluded DEMAND_HOTSPOTS
  case transporterConfig.demandHotspotsConfig of
    Just configs
      | configs.enableDemandHotspots && not demandHotspotsExcluded -> do
        let cachedResultKey = mkDemandHotspotCachedKey merchantOpCityId.getId
        cachedResult :: Maybe GetDemandHotspotsResp <- Redis.safeGet cachedResultKey
        case cachedResult of
          Just res -> do
            fork "Calculating demand hotpspots" $ do
              expirySec <- Redis.ttl cachedResultKey
              when (expirySec < 60) $ do
                void $ calculateDemandHotspots configs cachedResultKey True
            return res
          Nothing -> calculateDemandHotspots configs cachedResultKey False
    _ ->
      pure $
        GetDemandHotspotsResp
          { createdAt = now,
            expiryAt = now,
            hotspotsDetails = [],
            enableDemandHotspots = False
          }
  where
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
            finalResults <- mapM calculateResult $ take configs.noOfGeohashesToReturn $ sortBy (flip compare) $ filter (\(freq, _) -> freq > 0) freqWithSortedSetKey
            let resp =
                  GetDemandHotspotsResp
                    { createdAt = now,
                      expiryAt = T.addUTCTime (60 * fromIntegral configs.resultDurationMinutes) now,
                      hotspotsDetails = finalResults,
                      enableDemandHotspots = True
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
      let freq = length members
          (sumLat, sumLong) = foldr' (\member (accLat, accLong) -> (accLat + member.hotspotLat, accLong + member.hotspotLon)) (0, 0) members
          avgLatLong = LatLong (sumLat / fromIntegral freq) (sumLong / fromIntegral freq)
      return
        HotspotsDetails
          { frequency = freq,
            location = avgLatLong,
            categories = hotspotCategories members
          }

    hotspotCategories members =
      let sorted = sortBy (\a b -> compare (categoryKey a) (categoryKey b)) members
          grouped = groupBy (\a b -> categoryKey a == categoryKey b) sorted
       in map toCategory grouped
      where
        categoryKey member = (fromMaybe "Unknown" member.hotspotProduct, member.hotspotIsScheduled)
        toCategory (member : rest) =
          HotspotCategoryCount
            { tripOption = fromMaybe "Unknown" member.hotspotProduct,
              isScheduled = member.hotspotIsScheduled,
              frequency = 1 + length rest
            }
        toCategory [] = HotspotCategoryCount {tripOption = "Unknown", isScheduled = False, frequency = 0}

updateDemandHotspotsOnSearch :: Id SearchRequest -> Id MerchantOperatingCity -> TransporterConfig -> Maps.LatLong -> Text -> Bool -> Flow ()
updateDemandHotspotsOnSearch searchReqId merchantOpCityId transporterConfig latlong tripOption isScheduled = do
  now <- getCurrentTime
  demandHotspotsExcluded <- CQLF.isFeatureExcluded DEMAND_HOTSPOTS
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when (configs.enableDemandHotspots && not demandHotspotsExcluded) $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
              geohashSetKey = mkGeohashSetKey merchantOpCityId.getId
              expirationSecond = 60 * configs.analysisDurationMinutes
              object = mkHotspotObject searchReqId latlong (Just tripOption) isScheduled
          -- Missing/False → legacy 3-element (safe for old pods); Just True → 5-element.
          if fromMaybe False configs.storeHotspotProductInMember
            then Redis.zAdd sortedSetKey [(utcToMilliseconds now, object)]
            else Redis.zAdd sortedSetKey [(utcToMilliseconds now, LegacyHotspotObject object)]
          Redis.expire sortedSetKey expirationSecond
          Redis.sAddExp geohashSetKey [geohash] expirationSecond
    _ -> logDebug "Demand hotspots not enabled or configs not set on search"

-- Removes one search from its geohash.
-- Try exact 5-element, else exact 3-element, else zRange by search-id (coords/product may differ).
removeDemandHotspot ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id SearchRequest ->
  Id MerchantOperatingCity ->
  TransporterConfig ->
  Maps.LatLong ->
  Maybe Text ->
  Bool ->
  m ()
removeDemandHotspot searchReqId merchantOpCityId transporterConfig latlong mbProduct isScheduled = do
  demandHotspotsExcluded <- CQLF.isFeatureExcluded DEMAND_HOTSPOTS
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when (configs.enableDemandHotspots && not demandHotspotsExcluded) $ do
        let mbGeohash = Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)
        whenJust mbGeohash $ \geohash -> do
          let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
              searchId6 = T.take 6 searchReqId.getId
              object = mkHotspotObject searchReqId latlong mbProduct isScheduled
          removed5 <- case mbProduct of
            Just _ -> Redis.zRem sortedSetKey [cs $ Ae.encode object]
            Nothing -> pure 0
          unless (removed5 > 0) $ do
            removed3 <- Redis.zRem sortedSetKey [cs $ Ae.encode (LegacyHotspotObject object)]
            unless (removed3 > 0) $ do
              members <- Redis.zRange sortedSetKey 0 1000000
              let matching =
                    [ cs raw
                      | raw <- members,
                        Just HotspotObject {hotspotSearchId} <- [Ae.decode (cs raw)],
                        hotspotSearchId == searchId6
                    ]
              unless (null matching) $
                void $ Redis.zRem sortedSetKey matching
          logDebug $ "deleted demand hotspot member for search " <> searchId6
    _ -> logDebug "Demand hotspots not enabled or configs not set"

mkHotspotObject :: Id SearchRequest -> Maps.LatLong -> Maybe Text -> Bool -> HotspotObject
mkHotspotObject searchReqId latlong mbTripOption isScheduled =
  HotspotObject
    { hotspotSearchId = T.take 6 searchReqId.getId,
      hotspotLat = latlong.lat,
      hotspotLon = latlong.lon,
      hotspotProduct = mbTripOption,
      hotspotIsScheduled = isScheduled
    }

-- CK2: cached aggregate now includes the product breakdown. DH:CK still holds the old shape until it expires.
mkDemandHotspotCachedKey :: Text -> Text
mkDemandHotspotCachedKey opCityId = "DH:CK2:cityId:" <> opCityId

mkGeohashSetKey :: Text -> Text
mkGeohashSetKey opCityId = "DH:GSK:cityId:" <> opCityId

mkDemandHotspotSortedSetKey :: Text -> Text -> Text
mkDemandHotspotSortedSetKey opCityId geohash = "DH:cityId:" <> opCityId <> "GH:" <> geohash

mkHotspotsCalculationLockKey :: Text -> Text
mkHotspotsCalculationLockKey opCityId = "DH:CalcLock:cityId:" <> opCityId

-- | Returns active (unserved) search locations near a given point within radiusMeters.
-- Only reads the geohash buckets that overlap the bounding box of the circle —
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
  -- Filter by actual distance — bucket boundaries are rectangular, circle is round
  pure $ filter (isWithinRadius centerLatLong radiusMeters) rawEntries
  where
    processGeohash geohash = do
      let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId (T.pack geohash)
      rawItems <- Redis.zRangeByScore sortedSetKey cutoffScore (1 / 0 :: Double)
      pure $
        mapMaybe
          ( \r ->
              fmap
                (\member -> (member.hotspotSearchId, member.hotspotLat, member.hotspotLon))
                (Ae.decode $ cs r :: Maybe HotspotObject)
          )
          rawItems

    isWithinRadius cl r (_, slat, slon) =
      highPrecMetersToMeters (distanceBetweenInMeters cl (Maps.LatLong slat slon)) <= fromIntegral r

-- | Returns the full set of geohash cells that cover the circle defined by
-- center + radiusMeters. Samples a grid of points across the bounding box
-- at intervals of half the cell width — guarantees no interior cell is missed.
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

-- ===== Demand Nudge =====

data NudgeCellCache = NudgeCellCache
  { geohash :: Text,
    centroid :: LatLong,
    freshnessRatio :: Double,
    recentDemand :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Maps.HasCoordinates NudgeCellCache where
  getCoordinates cell = cell.centroid

mkNudgeCacheKey :: Text -> Text
mkNudgeCacheKey opCityId = "DH:NK:cityId:" <> opCityId

mkNudgeLockKey :: Text -> Text
mkNudgeLockKey opCityId = "DH:NudgeLock:cityId:" <> opCityId

postDriverDemandHotspot ::
  ( Maybe (Id Person),
    Id Merchant,
    Id MerchantOperatingCity
  ) ->
  DemandNudgeReq ->
  Flow APISuccess
postDriverDemandHotspot (mbDriverId, merchantId, merchantOpCityId) req = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  demandHotspotsExcluded <- CQLF.isFeatureExcluded DEMAND_HOTSPOTS
  case transporterConfig.demandHotspotsConfig of
    Just configs | configs.enableDemandHotspots && not demandHotspotsExcluded -> do
      let ageSecs = realToFrac (T.diffUTCTime now req.timestamp) :: Double
      unless (ageSecs > 120.0) $ do
        driverId <- fromMaybeM (InvalidRequest "Driver not logged in") mbDriverId
        driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
        let driverLatLon = LatLong req.lat req.lon
        fork "demandNudge" $ do
          mbActiveNudgeRideId <- Redis.safeGet (NudgeRide.nudgeRideRedisKey driverId) :: Flow (Maybe Text)
          unless (isJust mbActiveNudgeRideId) $ do
            cells <- computeNudgeCache merchantOpCityId configs
            unless (null cells) $ do
              let total = length cells
                  avgRecent = fromIntegral (sum (map (.recentDemand) cells)) / fromIntegral total :: Double
                  aboveAvg = filter (\c -> fromIntegral c.recentDemand > avgRecent) cells
                  candidates = if null aboveAvg then cells else aboveAvg
                  haversineScore c =
                    let distMeters = fromIntegral (highPrecMetersToMeters (distanceBetweenInMeters driverLatLon c.centroid)).getMeters :: Double
                        distKm = max 0.001 (distMeters / 1000.0)
                     in c.freshnessRatio * fromIntegral c.recentDemand / distKm
                  top20 = take 20 $ sortBy (flip compare `on` haversineScore) candidates
              mbWinner <- case NE.nonEmpty top20 of
                Nothing -> pure Nothing
                Just top20NE -> do
                  eitherDistances <-
                    withTryCatch "demandNudge:getDistances" $
                      TMaps.getDistancesOSRMOnly
                        merchantOpCityId
                        Nothing
                        Maps.GetDistancesReq
                          { origins = NE.fromList [driverLatLon],
                            destinations = top20NE,
                            distanceUnit = Meter,
                            sourceDestinationMapping = Nothing,
                            travelMode = Just Maps.CAR
                          }
                  pure $ case eitherDistances of
                    Right distances ->
                      let finalScore dr =
                            let travelMins = max 0.001 (fromIntegral dr.duration.getSeconds / 60.0 :: Double)
                             in dr.destination.freshnessRatio * fromIntegral dr.destination.recentDemand / travelMins
                       in Just (Kernel.Prelude.maximumBy (compare `on` finalScore) (NE.toList distances)).destination
                    Left _ -> Just (NE.head top20NE)
              whenJust mbWinner $ \winnerCell -> do
                snappedLoc <- do
                  eitherSnap <-
                    withTryCatch "demandNudge:snapToRoad" $
                      TMaps.snapToRoadOSRMOnly
                        merchantOpCityId
                        Nothing
                        Maps.SnapToRoadReq
                          { points = [winnerCell.centroid],
                            distanceUnit = Meter,
                            calculateDistanceFrom = Nothing
                          }
                  pure $ case eitherSnap of
                    Right (_, Right snapResp) ->
                      case snapResp.snappedPoints of
                        h : _ -> h
                        [] -> winnerCell.centroid
                    _ -> winnerCell.centroid
                mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId "DEMAND_NUDGE" (fromMaybe ENGLISH driver.language) Nothing Nothing Nothing
                whenJust mbOverlay $ \overlay -> do
                  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
                  vehicle <- QVeh.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
                  void $ NudgeRide.createNudgeBookingAndRide merchant driver vehicle merchantOpCityId driverLatLon snappedLoc overlay
    _ -> pure ()
  pure Success

computeNudgeCache :: Id MerchantOperatingCity -> DemandHotspotsConfig -> Flow [NudgeCellCache]
computeNudgeCache merchantOpCityId configs = do
  let nudgeCacheKey = mkNudgeCacheKey merchantOpCityId.getId
  cachedCells :: Maybe [NudgeCellCache] <- Redis.safeGet nudgeCacheKey
  case cachedCells of
    Just cells -> do
      fork "nudgeCacheRefresh" $ do
        expirySec <- Redis.ttl nudgeCacheKey
        when (expirySec < 60) $
          void $ buildNudgeCache nudgeCacheKey True
      return cells
    Nothing -> buildNudgeCache nudgeCacheKey False
  where
    buildNudgeCache nudgeCacheKey forceRebuild =
      Redis.withWaitAndLockRedis (mkNudgeLockKey merchantOpCityId.getId) 10 10000 $ do
        recheckCells :: Maybe [NudgeCellCache] <- if forceRebuild then return Nothing else Redis.safeGet nudgeCacheKey
        case recheckCells of
          Just cells -> return cells
          Nothing -> do
            now <- getCurrentTime
            activeGeohashes <- Redis.sMembers (mkGeohashSetKey merchantOpCityId.getId)
            let expiryTime = T.addUTCTime (- (60 * fromIntegral configs.analysisDurationMinutes)) now
                recentStart = T.addUTCTime (-300) now
                cacheTtlSecs = 5 * 60
            cells <- catMaybes <$> mapM (processOneGeohash expiryTime recentStart) activeGeohashes
            unless (null cells) $
              Redis.setExp nudgeCacheKey cells cacheTtlSecs
            return cells

    processOneGeohash expiryTime recentStart geohash = do
      let sortedSetKey = mkDemandHotspotSortedSetKey merchantOpCityId.getId geohash
      void $ Redis.zRemRangeByScore sortedSetKey 0 (utcToMilliseconds expiryTime)
      totalDemand <- fmap integerToInt (Redis.zCard sortedSetKey)
      if totalDemand == 0
        then do
          void $ Redis.srem (mkGeohashSetKey merchantOpCityId.getId) [geohash]
          return Nothing
        else do
          recentDemandCount <- fmap integerToInt (Redis.zCount sortedSetKey (utcToMilliseconds recentStart) (1 / 0 :: Double))
          let freshnessRatio = fromIntegral recentDemandCount / fromIntegral totalDemand :: Double
          if freshnessRatio < 0.40
            then return Nothing
            else do
              rawMembers <- Redis.zRange sortedSetKey 0 1000000
              let members :: [HotspotObject] = mapMaybe (Ae.decode . cs) rawMembers
              case members of
                [] -> return Nothing
                _ -> do
                  let n = length members
                      (sumLat, sumLon) = foldr' (\m (al, an) -> (al + m.hotspotLat, an + m.hotspotLon)) (0.0, 0.0) members
                      centroid = LatLong (sumLat / fromIntegral n) (sumLon / fromIntegral n)
                  return $
                    Just
                      NudgeCellCache
                        { geohash = geohash,
                          centroid = centroid,
                          freshnessRatio = freshnessRatio,
                          recentDemand = recentDemandCount
                        }
