module Domain.Action.UI.DemandHotspots
  ( getDriverDemandHotspots,
    tripOptionFromCategories,
    updateDemandHotspotsOnSearch,
    removeDemandHotspot,
    updateDemandHotspotMultiplierObservations,
    getActiveSearchLocations,
  )
where

import API.Types.UI.DemandHotspots
import Data.Aeson (withArray)
import qualified Data.Aeson as Ae
import qualified Data.Geohash as Geohash
import Data.List (groupBy, nub)
import qualified Data.Map.Strict as Map
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Time as T
import qualified Data.Vector as V
import qualified Domain.Types.Estimate as DEst
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.SearchRequest
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.TransporterConfig
import qualified Domain.Types.Trip as Trip
import Domain.Types.VehicleVariant (castVariantToServiceTier)
import Environment
import EulerHS.Prelude hiding (foldr', groupBy, id, length, map, mapM_, null, sum, whenJust)
import GHC.Num.Integer (integerToInt)
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
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Vehicle as QVehicle

-- What it is:
-- The city is divided into geohashes. Each cell holds a sorted set of the searches whose riders
-- are committed to a trip there but have not started it, so a hotspot is unserved demand in that cell.
--
-- How it works:
--
-- Redis data structures we are using - set and sorted set
-- Set - For storing all geohashes we have in a city
-- Sorted Set - Storing searches categorised by cityId and geohash
--
-- On Select     - EstimateBased flows: the rider tapped a fare, so we add the search to the cell's
--                  sorted set. QuoteBased flows (airport OTP, rental) have no Select, so they're
--                  added on Search instead.
-- On Ride start - We remove that search from the cell's sorted set (removeDemandHotspot).
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
--
-- Each hotspot also carries a congestion fare multiplier, built from real committed demand.
-- On Select - For every estimate the rider selected we bump a count for its (service tier,
--   multiplier bin) into a per-cell time-bucketed histogram (DH:mult).
-- Recompute - For the returned cells we read the last few buckets and take the count-weighted
--   mean multiplier per service tier, cached aligned with the hotspot list (DH:multByTier).
-- On Read - We blend the means of the tiers the driver serves, weighted by count.

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
  [] -> "Unknown"

getDriverDemandHotspots ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Merchant,
      Id MerchantOperatingCity
    ) ->
    Flow GetDemandHotspotsResp
  )
getDriverDemandHotspots (mbPersonId, _, merchantOpCityId) = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  case transporterConfig.demandHotspotsConfig of
    Just configs
      | configs.enableDemandHotspots -> do
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
        -- Frequency and location are cached per city. The multiplier depends on the driver's
        -- service tiers, so it is blended in here once the driver is known.
        overlayCongestionMultiplier res
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
            freqWithSortedSetKey :: [(Int, (Text, Text))] <- mapM getFrequencyWithSSKey allSortedSetsKeysWithGeohash
            finalResults :: [(HotspotsDetails, Text)] <- mapM calculateResult $ take configs.noOfGeohashesToReturn $ sortBy (flip compare) $ filter (\(freq, _) -> freq > 0) freqWithSortedSetKey
            multByTier :: [Map.Map Text (Double, Int)] <- mapM (\(_, geohash) -> multiplierByTierForGeohash now geohash) finalResults
            Redis.setExp (mkDemandHotspotMultByTierKey merchantOpCityId.getId) multByTier (60 * configs.resultDurationMinutes)
            let resp =
                  GetDemandHotspotsResp
                    { createdAt = now,
                      expiryAt = T.addUTCTime (60 * fromIntegral configs.resultDurationMinutes) now,
                      hotspotsDetails = map fst finalResults,
                      enableDemandHotspots = True
                    }
            Redis.setExp cachedResultKey resp (60 * configs.resultDurationMinutes)
            return resp

    getFrequencyWithSSKey (sortedSetKey, geohash) = do
      frequency <- fmap integerToInt (Redis.zCard sortedSetKey)
      when (frequency == 0) $ do
        void $ Redis.srem (mkGeohashSetKey merchantOpCityId.getId) [geohash]
      return (frequency, (sortedSetKey, geohash))

    calculateResult (_, (sortedSetKey, geohash)) = do
      res <- Redis.zRange sortedSetKey 0 1000000
      members :: [HotspotObject] <- mapM (\a -> fromMaybeM (HedisDecodeError $ cs a) . Ae.decode $ cs a) res
      let freq = length members
          (sumLat, sumLong) = foldr' (\member (accLat, accLong) -> (accLat + member.hotspotLat, accLong + member.hotspotLon)) (0, 0) members
          avgLatLong = LatLong (sumLat / fromIntegral freq) (sumLong / fromIntegral freq)
      return
        ( HotspotsDetails
            { frequency = freq,
              location = avgLatLong,
              categories = hotspotCategories members,
              multiplier = Nothing
            },
          geohash
        )

    overlayCongestionMultiplier res = do
      mbServiceTiers <- resolveDriverServiceTiers
      case mbServiceTiers of
        Nothing -> pure res
        Just serviceTiers -> do
          mbAligned :: Maybe [Map.Map Text (Double, Int)] <- Redis.safeGet (mkDemandHotspotMultByTierKey merchantOpCityId.getId)
          case mbAligned of
            Just aligned
              | length aligned == length res.hotspotsDetails ->
                pure res {hotspotsDetails = zipWith (attachMultiplier serviceTiers) res.hotspotsDetails aligned}
            _ -> pure res

    resolveDriverServiceTiers = case mbPersonId of
      Nothing -> pure Nothing
      Just personId -> do
        mbVehicle <- QVehicle.findById personId
        pure $
          ( \v -> case v.selectedServiceTiers of
              [] -> [castVariantToServiceTier v.variant]
              tiers -> tiers
          )
            <$> mbVehicle

    attachMultiplier serviceTiers hotspot tierMap =
      let picked = mapMaybe (\tier -> Map.lookup (show tier) tierMap) serviceTiers
          totalCount = sum (map snd picked)
       in if totalCount == 0
            then hotspot
            else hotspot {multiplier = Just (sum (map (\(avg, count) -> avg * fromIntegral count) picked) / fromIntegral totalCount)}

    -- For one cell, read the last few histogram buckets and take the count-weighted mean
    -- multiplier per service tier, keeping only tiers with enough observations.
    multiplierByTierForGeohash now geohash = do
      let bucketWidth = fromIntegral multiplierBucketWidthSec :: Integer
          nowSec = floor (utcToMilliseconds now / 1000) :: Integer
          currentBucket = (nowSec `div` bucketWidth) * bucketWidth
          buckets = [currentBucket - fromIntegral i * bucketWidth | i <- [0 .. multiplierLookbackBuckets - 1]]
      triples <- concat <$> mapM (readMultiplierHistogram geohash) buckets
      let byTier =
            Map.fromListWith
              (\(s1, c1) (s2, c2) -> (s1 + s2, c1 + c2))
              [(tier, (binValue * fromIntegral count, count)) | (tier, binValue, count) <- triples]
      pure $
        Map.mapMaybe
          (\(weightedSum, total) -> if total >= multiplierMinCount then Just (weightedSum / fromIntegral total, total) else Nothing)
          byTier

    readMultiplierHistogram geohash bucket = do
      raw <- Redis.zRangeWithScores (mkDemandHotspotMultBucketKey merchantOpCityId.getId geohash bucket) 0 (-1)
      pure $ mapMaybe decodeMultiplierMember raw

    decodeMultiplierMember (memberBs, score) =
      (\(tier, binValue) -> (tier, fromIntegral binValue / 100.0, round score :: Int))
        <$> (Ae.decode (cs memberBs) :: Maybe (Text, Int))

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
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
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
  case transporterConfig.demandHotspotsConfig of
    Just configs -> do
      when configs.enableDemandHotspots $ do
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

-- For every estimate the rider selected we bump a count for its (service tier, multiplier bin)
-- into a per-cell time-bucketed histogram. Nothing is removed - a bucket ages out with its key.
-- A Select can legitimately be reprocessed for the same (search, estimate) pair (addOns resent,
-- a retry that picked up a fresh messageId), so each estimate is only ever counted once via an
-- atomic claim on its own dedup key.
updateDemandHotspotMultiplierObservations ::
  Id SearchRequest ->
  Id MerchantOperatingCity ->
  TransporterConfig ->
  Maps.LatLong ->
  [(Id DEst.Estimate, DVST.ServiceTierType, Double)] ->
  Flow ()
updateDemandHotspotMultiplierObservations searchReqId merchantOpCityId transporterConfig latlong tierMultipliers = do
  now <- getCurrentTime
  case transporterConfig.demandHotspotsConfig of
    Just configs | configs.enableDemandHotspots -> do
      let ttlSeconds = multiplierBucketWidthSec * (multiplierLookbackBuckets + 1)
      newObservations <- filterM (\(estimateId, _, _) -> Redis.setNxExpire (mkMultiplierDedupKey searchReqId estimateId) ttlSeconds True) tierMultipliers
      case newObservations of
        [] -> pure ()
        _ ->
          whenJust (Geohash.encode configs.precisionOfGeohash (latlong.lat, latlong.lon)) $ \geohash -> do
            let bucketWidth = fromIntegral multiplierBucketWidthSec :: Integer
                nowSec = floor (utcToMilliseconds now / 1000) :: Integer
                bucket = (nowSec `div` bucketWidth) * bucketWidth
                histogramKey = mkDemandHotspotMultBucketKey merchantOpCityId.getId (T.pack geohash) bucket
            mapM_ (\(_, tier, multiplier) -> Redis.zIncrBy histogramKey 1 (multiplierMember tier multiplier)) newObservations
            Redis.expire histogramKey ttlSeconds
    _ -> logDebug "Demand hotspots not enabled or configs not set on multiplier observation"

multiplierMember :: DVST.ServiceTierType -> Double -> (Text, Int)
multiplierMember tier multiplier =
  (show tier, round (multiplier * 100 / fromIntegral multiplierBinPercent) * multiplierBinPercent)

-- Histogram bucket width in seconds.
multiplierBucketWidthSec :: Int
multiplierBucketWidthSec = 300

-- How many buckets back the recompute reads.
multiplierLookbackBuckets :: Int
multiplierLookbackBuckets = 2

-- Minimum observations for a tier before its mean is shown.
multiplierMinCount :: Int
multiplierMinCount = 5

-- Multiplier bin width, as a percentage. 5 means 0.05-wide bins.
multiplierBinPercent :: Int
multiplierBinPercent = 5

-- CK2: cached aggregate now includes the product breakdown. DH:CK still holds the old shape until it expires.
mkDemandHotspotCachedKey :: Text -> Text
mkDemandHotspotCachedKey opCityId = "DH:CK2:cityId:" <> opCityId

mkGeohashSetKey :: Text -> Text
mkGeohashSetKey opCityId = "DH:GSK:cityId:" <> opCityId

mkDemandHotspotSortedSetKey :: Text -> Text -> Text
mkDemandHotspotSortedSetKey opCityId geohash = "DH:cityId:" <> opCityId <> "GH:" <> geohash

mkHotspotsCalculationLockKey :: Text -> Text
mkHotspotsCalculationLockKey opCityId = "DH:CalcLock:cityId:" <> opCityId

mkDemandHotspotMultBucketKey :: Text -> Text -> Integer -> Text
mkDemandHotspotMultBucketKey opCityId geohash bucket = "DH:mult:cityId:" <> opCityId <> ":GH:" <> geohash <> ":" <> show bucket

mkDemandHotspotMultByTierKey :: Text -> Text
mkDemandHotspotMultByTierKey opCityId = "DH:multByTier:cityId:" <> opCityId

mkMultiplierDedupKey :: Id SearchRequest -> Id DEst.Estimate -> Text
mkMultiplierDedupKey searchReqId estimateId = "DH:multDedup:searchId:" <> searchReqId.getId <> ":estimateId:" <> estimateId.getId

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
