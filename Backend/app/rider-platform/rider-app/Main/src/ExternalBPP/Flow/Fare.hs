module ExternalBPP.Flow.Fare where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.List.NonEmpty as NE
import Domain.Types.FRFSQuote as DFRFSQuote
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto.Config as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FRFSUtils
import qualified SharedLogic.PTCircuitBreaker as CB
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Prelude as P

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Id Merchant -> Id MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty CallAPI.BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe Text -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> Bool -> m (Bool, [FRFSFare])
getFares riderId merchantId merchantOperatingCityId integratedBPPConfig fareRouteDetails vehicleCategory serviceTier mbParentSearchReqId blacklistedServiceTiers blacklistedFareQuoteTypes getAllSubwayFares isSingleMode = do
  subwayFareDetail <-
    case integratedBPPConfig.providerConfig of
      CRIS _ -> do
        if getAllSubwayFares
          then
            return $
              Just
                CallAPI.SubwayFareDetail
                  { viaPoints = " ",
                    changeOver = " ",
                    rawChangeOver = " ",
                    getAllFares = True
                  }
          else do
            (viaPoints, changeOver, rawChangeOver) <- CallAPI.getChangeOverAndViaPoints (NE.toList fareRouteDetails) integratedBPPConfig
            return $
              Just
                CallAPI.SubwayFareDetail
                  { viaPoints = viaPoints,
                    changeOver = changeOver,
                    rawChangeOver = rawChangeOver,
                    getAllFares = False
                  }
      _ -> return Nothing

  -- Circuit breaker check
  let ptMode = CB.vehicleCategoryToPTMode vehicleCategory
  mRiderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing
  let circuitOpen = CB.isCircuitOpen ptMode CB.FareAPI mRiderConfig
  let cbConfig = CB.parseCircuitBreakerConfig (mRiderConfig >>= (.ptCircuitBreakerConfig))
  let apiConfig = cbConfig.fare

  -- Extract route details using pattern matching (safe for NonEmpty)
  let (firstRoute NE.:| restRoutes) = fareRouteDetails
  let lastRoute = fromMaybe firstRoute (listToMaybe $ reverse restRoutes)

      baseCacheKey =
        CB.makeFareCacheKey
          merchantOperatingCityId
          ptMode
          (getCacheRouteCode firstRoute)
          firstRoute.startStopCode lastRoute.endStopCode
          serviceTier
      setCacheKey = buildCacheKeySuffix subwayFareDetail <&> \suffix -> (baseCacheKey <> suffix, getCacheTTL)
      getCacheKey = buildCacheKeySuffix subwayFareDetail <&> \suffix -> (baseCacheKey <> suffix, getCacheFilterFn subwayFareDetail)

  -- Get threshold for 2x probing
  let failureThreshold = CB.getFirstThresholdFailureCount apiConfig
  let probeLimit = 2 * failureThreshold

  -- Determine if we should always probe (for CRIS non-single-mode)
  let alwaysProbe =
        case integratedBPPConfig.providerConfig of
          CRIS _ -> not isSingleMode
          _ -> False
      isFareMandatory =
        case integratedBPPConfig.providerConfig of
          ONDC _ -> False
          _ -> True

  fares <-
    if circuitOpen
      then handleCircuitOpen cbConfig ptMode setCacheKey getCacheKey subwayFareDetail apiConfig
      else handleCircuitClosed cbConfig ptMode setCacheKey getCacheKey subwayFareDetail probeLimit apiConfig alwaysProbe
  return $ (isFareMandatory, filterBlacklistedFaresForUI fares)
  where
    -- Helper functions for cache key construction and metadata
    buildCacheKeySuffix :: Maybe CallAPI.SubwayFareDetail -> Maybe Text
    buildCacheKeySuffix subwayFareDetail = case integratedBPPConfig.providerConfig of
      CRIS _ -> case (mbParentSearchReqId, isSingleMode) of
        (Just parentSearchReqId, True) -> Just $ ":" <> parentSearchReqId
        (_, False) -> Just $ maybe "" (\sd -> ":" <> sd.rawChangeOver) subwayFareDetail
        _ -> Nothing
      _ -> Just ""

    getCacheTTL :: Int
    getCacheTTL = case integratedBPPConfig.providerConfig of
      CRIS _ -> if isSingleMode then 1800 else 3600
      _ -> 1800

    getCacheRouteCode :: CallAPI.BasicRouteDetail -> Text
    getCacheRouteCode firstRoute = case integratedBPPConfig.providerConfig of
      CRIS _ -> "-"
      _ -> firstRoute.routeCode

    getCacheFilterFn :: Maybe CallAPI.SubwayFareDetail -> ([FRFSFare] -> [FRFSFare])
    getCacheFilterFn subwayFareDetail = case integratedBPPConfig.providerConfig of
      CRIS _ -> case (subwayFareDetail, isSingleMode) of
        (Just subwayFareDetail', True) ->
          \fares -> filter (\fare -> maybe False (\fd -> fd.via == subwayFareDetail'.rawChangeOver) fare.fareDetails) fares
        _ -> P.id
      _ -> P.id

    -- When circuit is OPEN: clear cache and try canary request
    handleCircuitOpen cbConfig ptMode setCacheKey getCacheKey subwayFareDetail apiConfig = do
      let canaryAllowed = fromMaybe 2 (apiConfig <&> (.canaryAllowedPerWindow))
      let canaryWindow = fromMaybe 60 (apiConfig <&> (.canaryWindowSeconds))
      canarySlot <- CB.tryAcquireCanarySlot ptMode CB.FareAPI merchantOperatingCityId canaryAllowed canaryWindow

      if canarySlot
        then do
          logInfo $ "PT Circuit Breaker: Canary request for " <> show ptMode
          -- Make the canary API call
          callFareAPI cbConfig True setCacheKey getCacheKey subwayFareDetail
        else return [] -- Circuit open, no canary slot

    -- When circuit is CLOSED: use cache with background probing (fallback through multiple cache keys)
    handleCircuitClosed cbConfig ptMode setCacheKey getCacheKey subwayFareDetail probeLimit apiConfig alwaysProbe = do
      -- Try to get from caches in fallback manner (filter applied inside tryGetFromCaches)
      cachedResult <- tryGetFromCaches getCacheKey

      case cachedResult of
        Just filteredFares | not (null filteredFares) -> do
          let probeWindow = fromMaybe 60 (apiConfig <&> (.canaryWindowSeconds))
          probeCount <- CB.getProbeCounter ptMode CB.FareAPI merchantOperatingCityId
          let shouldProbe = case integratedBPPConfig.providerConfig of
                CRIS _ -> not isSingleMode
                _ -> probeCount < probeLimit
          when (alwaysProbe || shouldProbe) $ do
            -- Fork a background probe request for circuit breaker tracking
            fork "probe-fare-api" $ do
              unless alwaysProbe $ void $ CB.incrementProbeCounter ptMode CB.FareAPI merchantOperatingCityId probeWindow
              result <-
                withTryCatch "probe:getFares" $
                  CallAPI.getFares
                    riderId
                    merchantId
                    merchantOperatingCityId
                    integratedBPPConfig
                    fareRouteDetails
                    vehicleCategory
                    serviceTier
                    subwayFareDetail
              case result of
                Left _ -> do
                  CB.recordFailure ptMode CB.FareAPI merchantOperatingCityId
                  CB.checkAndDisableIfNeeded ptMode CB.FareAPI merchantOperatingCityId cbConfig
                Right freshFares -> do
                  CB.recordSuccess ptMode CB.FareAPI merchantOperatingCityId
                  -- Update all caches with fresh data if successful (store unfiltered)
                  unless (null freshFares) $
                    setToCaches setCacheKey freshFares
          return filteredFares
        _ -> do
          -- No cached fares: call API and cache result
          callFareAPI cbConfig False setCacheKey getCacheKey subwayFareDetail

    -- Try getting from cache key with optional filter
    tryGetFromCaches :: (MonadFlow m, CacheFlow m r) => Maybe (Text, [FRFSFare] -> [FRFSFare]) -> m (Maybe [FRFSFare])
    tryGetFromCaches Nothing = pure Nothing
    tryGetFromCaches (Just (key, filterFn)) = do
      cachedFares <- Redis.safeGet key
      case cachedFares of
        Just fares | not (null fares) -> return $ Just $ filterFn fares
        _ -> return Nothing

    -- Set to cache with TTL
    setToCaches :: (MonadFlow m, CacheFlow m r) => Maybe (Text, Int) -> [FRFSFare] -> m ()
    setToCaches Nothing _ = pure ()
    setToCaches (Just (key, ttl)) fares = Redis.setExp key fares ttl

    -- Helper to call fare API with circuit breaker tracking
    callFareAPI cbConfig isCanary setCacheKey getCacheKey subwayFareDetail = do
      let ptMode = CB.vehicleCategoryToPTMode vehicleCategory
      result <-
        withTryCatch "callExternalBPP:getFares" $
          CallAPI.getFares
            riderId
            merchantId
            merchantOperatingCityId
            integratedBPPConfig
            fareRouteDetails
            vehicleCategory
            serviceTier
            subwayFareDetail

      case result of
        Left _ -> do
          CB.recordFailure ptMode CB.FareAPI merchantOperatingCityId
          CB.checkAndDisableIfNeeded ptMode CB.FareAPI merchantOperatingCityId cbConfig
          -- When circuit opens, clear cache
          circuitNowOpen <- CB.isCircuitOpen ptMode CB.FareAPI <$> QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing
          when circuitNowOpen $ do
            whenJust getCacheKey $ CB.clearFareCache . fst
            CB.resetProbeCounter ptMode CB.FareAPI merchantOperatingCityId
          return []
        Right fares -> do
          -- If this was a canary request and it succeeded, re-enable the circuit
          when isCanary $ do
            CB.reEnableCircuit ptMode CB.FareAPI merchantOperatingCityId
            CB.resetProbeCounter ptMode CB.FareAPI merchantOperatingCityId

          CB.recordSuccess ptMode CB.FareAPI merchantOperatingCityId

          -- Cache the fares to all cache keys (store unfiltered)
          unless (null fares) $
            setToCaches setCacheKey fares
          return fares

    -- Filter fares based on blacklisted service tiers and quote types that UI cannot parse
    filterBlacklistedFaresForUI :: [FRFSFare] -> [FRFSFare]
    filterBlacklistedFaresForUI fares =
      filter
        ( \fare ->
            notElem fare.vehicleServiceTier.serviceTierType blacklistedServiceTiers
              && maybe True (\ft -> notElem ft blacklistedFareQuoteTypes) fare.fareQuoteType
        )
        fares
