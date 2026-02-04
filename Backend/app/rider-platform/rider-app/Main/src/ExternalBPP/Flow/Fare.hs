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
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Prelude as P

getFares :: (CoreMetrics m, CacheFlow m r, EsqDBFlow m r, DB.EsqDBReplicaFlow m r, EncFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c) => Id Person -> Id Merchant -> Id MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty CallAPI.BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe Text -> [Spec.ServiceTierType] -> [DFRFSQuote.FRFSQuoteType] -> Bool -> m (Bool, [FRFSFare])
getFares riderId merchantId merchantOperatingCityId integratedBPPConfig fareRouteDetails vehicleCategory serviceTier mbParentSearchReqId blacklistedServiceTiers blacklistedFareQuoteTypes getAllSubwayFares = do
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
  mRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, txnId = Nothing})
  let circuitOpen = CB.isCircuitOpen ptMode CB.FareAPI mRiderConfig
  let cbConfig = CB.parseCircuitBreakerConfig (mRiderConfig >>= (.ptCircuitBreakerConfig))
  let apiConfig = cbConfig.fare

  -- Extract route details using pattern matching (safe for NonEmpty)
  let (firstRoute NE.:| restRoutes) = fareRouteDetails
  let lastRoute = fromMaybe firstRoute (listToMaybe $ reverse restRoutes)

  -- Build cache key - for CRIS include searchId for session-specific caching
  let baseCacheKey =
        CB.makeFareCacheKey
          merchantOperatingCityId
          ptMode
          firstRoute.routeCode firstRoute.startStopCode lastRoute.endStopCode
          serviceTier
      setCacheKeys = case integratedBPPConfig.providerConfig of
        CRIS _ ->
          ( case (mbParentSearchReqId, getAllSubwayFares) of
              (Just parentSearchReqId, True) -> [baseCacheKey <> ":" <> parentSearchReqId]
              _ -> []
          )
            <> [ baseCacheKey
                   <> maybe "" (\subwayFareDetail' -> ":" <> subwayFareDetail'.changeOver) subwayFareDetail
               ]
        _ -> [baseCacheKey]
      getCacheKeys = case integratedBPPConfig.providerConfig of
        CRIS _ ->
          ( case (mbParentSearchReqId, subwayFareDetail) of
              (Just parentSearchReqId, Just subwayFareDetail') -> [(baseCacheKey <> ":" <> parentSearchReqId, \fares -> filter (\fare -> maybe False (\fd -> fd.via == subwayFareDetail'.rawChangeOver) fare.fareDetails) fares)]
              _ -> []
          )
            <> [ ( baseCacheKey
                     <> maybe "" (\subwayFareDetail' -> ":" <> subwayFareDetail'.changeOver) subwayFareDetail,
                   P.id
                 )
               ]
        _ -> [(baseCacheKey, P.id)]

  -- Get threshold for 2x probing
  let failureThreshold = CB.getFirstThresholdFailureCount apiConfig
  let probeLimit = 2 * failureThreshold

  -- Determine if we should always probe (for Suburban)
  let alwaysProbe =
        case integratedBPPConfig.providerConfig of
          CRIS _ -> True
          _ -> False
      isFareMandatory =
        case integratedBPPConfig.providerConfig of
          ONDC _ -> False
          _ -> True

  fares <-
    if circuitOpen
      then handleCircuitOpen cbConfig ptMode setCacheKeys getCacheKeys subwayFareDetail apiConfig
      else handleCircuitClosed cbConfig ptMode setCacheKeys getCacheKeys subwayFareDetail probeLimit apiConfig alwaysProbe
  return $ (isFareMandatory, filterBlacklistedFaresForUI fares)
  where
    -- When circuit is OPEN: clear cache and try canary request
    handleCircuitOpen cbConfig ptMode setCacheKeys getCacheKeys subwayFareDetail apiConfig = do
      let canaryAllowed = fromMaybe 2 (apiConfig <&> (.canaryAllowedPerWindow))
      let canaryWindow = fromMaybe 60 (apiConfig <&> (.canaryWindowSeconds))
      canarySlot <- CB.tryAcquireCanarySlot ptMode CB.FareAPI merchantOperatingCityId canaryAllowed canaryWindow

      if canarySlot
        then do
          logInfo $ "PT Circuit Breaker: Canary request for " <> show ptMode
          -- Make the canary API call
          callFareAPI cbConfig True setCacheKeys getCacheKeys subwayFareDetail
        else return [] -- Circuit open, no canary slot

    -- When circuit is CLOSED: use cache with background probing (fallback through multiple cache keys)
    handleCircuitClosed cbConfig ptMode setCacheKeys getCacheKeys subwayFareDetail probeLimit apiConfig alwaysProbe = do
      -- Try to get from caches in fallback manner (filter applied inside tryGetFromCaches)
      cachedResult <- tryGetFromCaches getCacheKeys

      case cachedResult of
        Just filteredFares | not (null filteredFares) -> do
          -- Cached fares available: fork probing calls in background
          let probeWindow = fromMaybe 60 (apiConfig <&> (.canaryWindowSeconds))
          probeCount <- CB.getProbeCounter ptMode CB.FareAPI merchantOperatingCityId
          -- For Suburban (alwaysProbe=True), skip probe limit check
          when (alwaysProbe || probeCount < probeLimit) $ do
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
                    setToCaches setCacheKeys freshFares
          return filteredFares
        _ -> do
          -- No cached fares: call API and cache result
          callFareAPI cbConfig False setCacheKeys getCacheKeys subwayFareDetail

    -- Try getting from multiple cache keys in fallback order, apply filter and fallback if empty after filter
    tryGetFromCaches :: (MonadFlow m, CacheFlow m r) => [(Text, [FRFSFare] -> [FRFSFare])] -> m (Maybe [FRFSFare])
    tryGetFromCaches [] = return Nothing
    tryGetFromCaches ((key, filterFn) : rest) = do
      cachedFares <- Redis.safeGet key
      case cachedFares of
        Just fares | not (null fares) -> do
          let filteredFares = filterFn fares
          if null filteredFares
            then tryGetFromCaches rest -- Filter returned empty, try next cache
            else return $ Just filteredFares
        _ -> tryGetFromCaches rest

    -- Set to multiple cache keys
    setToCaches :: (MonadFlow m, CacheFlow m r) => [Text] -> [FRFSFare] -> m ()
    setToCaches keys fares =
      forM_ keys $ \key -> Redis.setExp key fares 1800 -- 30 min TTL

    -- Helper to call fare API with circuit breaker tracking
    callFareAPI cbConfig isCanary setCacheKeys getCacheKeys subwayFareDetail = do
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
          circuitNowOpen <- CB.isCircuitOpen ptMode CB.FareAPI <$> getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, txnId = Nothing})
          when circuitNowOpen $ do
            CB.clearFareCache (fst <$> getCacheKeys)
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
            setToCaches setCacheKeys fares
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
