{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.RiderPooling
  ( GSIMember (..),
    ValidationStep (..),
    handleSyncRiderPooling,
    addToSharedRideGSI,
    validateFinalCapacityAndGrouping,
    createSharedEntityForFinalMatch,
    isValidCandidate,
    parseGSIMember,
    calculateDistanceFromCoords,
    getActualDistance,
    generateLockKey,
    getVehicleCapacity,
    checkLockStatus,
    checkExpiryTime,
    checkSeatAvailability,
    checkDropoffDistanceCompatibility,
    checkRouteOverlapCompatibility,
    getRouteHashesFromCache,
    extractRoutePointsFromMapsResponse,
    findOptimalGrouping,
    trySingleCandidate,
    tryPairCombination,
    tryMultipleCombination,
    greedyPackingAlgorithm,
  )
where

import Control.Monad.Extra (filterM)
import Data.Aeson as A
import qualified Data.Geohash as Geohash
import Data.List (find, maximumBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Database.Redis as Hedis
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SharedEntity as DSE
import Domain.Types.SharedRideConfigs (SharedRideConfigs)
import Kernel.External.Maps.HasCoordinates (getCoordinates)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.SharedEntity as QSharedEntity
import Tools.Error
import Tools.Maps (GetRoutesResp)
import qualified Tools.Maps as Maps

-- Data types for rider pooling implementation
data GSIMember = GSIMember
  { gsiEstimateId :: Id DEstimate.Estimate,
    gsiValidTill :: UTCTime,
    gsiNumSeats :: Int,
    gsiCoordinates :: (Double, Double)
  }
  deriving (Generic, Show)

-- Data type for validation step results
data ValidationStep = ValidationStep
  { stepName :: Text,
    isValid :: Bool,
    reason :: Maybe Text
  }

-- Parse GSI member from Redis response
parseGSIMember :: Text -> UTCTime -> (Double, Double) -> Maybe GSIMember
parseGSIMember memberText now coords = do
  case T.splitOn ":" memberText of
    [estimateIdText, validTillText, numSeatsText] -> do
      let estimateId = Id estimateIdText
      validTill <- parseTimeM True defaultTimeLocale "%s" (T.unpack validTillText)
      numSeats <- readMaybe (T.unpack numSeatsText)
      return $ GSIMember estimateId validTill numSeats coords
    _ -> Nothing

-- Calculate straight-line distance between two coordinates (for initial filtering)
calculateDistanceFromCoords :: (Double, Double) -> (Double, Double) -> Distance
calculateDistanceFromCoords (lat1, lon1) (lat2, lon2) =
  let point1 = LatLong lat1 lon1
      point2 = LatLong lat2 lon2
   in Distance (distanceBetweenInMeters point1 point2) Meter

-- Get actual route-based distance using Maps service for Location types
getActualDistance ::
  (ServiceFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DLocation.Location ->
  DLocation.Location ->
  m Distance
getActualDistance merchantId merchantOpCityId origin destination = do
  let req =
        GetDistanceReq
          { origin = origin,
            destination = destination,
            travelMode = Just CAR,
            distanceUnit = Meter,
            sourceDestinationMapping = Nothing
          }
  response <- Maps.getDistance merchantId merchantOpCityId Nothing req
  return response.distance

-- Generate lock key for estimate
generateLockKey :: Id DEstimate.Estimate -> Text
generateLockKey estimateId = "shared_ride_lock:" <> estimateId.getId

-- Add customer to shared ride GSI for pooling
addToSharedRideGSI ::
  (MonadFlow m, CacheFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  m ()
addToSharedRideGSI config searchRequest estimate numSeats = do
  let gsiKey = "ShareRideCustomerLoc" -- GSI key from sharedRideFlowCore.md
  now <- getCurrentTime

  -- Calculate expiry time: config time - buffer time
  let expirySeconds = config.searchRequestExpirySeconds.getSeconds - config.searchExpiryBufferSeconds.getSeconds
  let validTill = addUTCTime (fromIntegral expirySeconds) now

  -- Use estimateId instead of searchRequestId for better tracking
  let memberKey = estimate.id.getId <> ":" <> show validTill <> ":" <> show numSeats
  let lat = searchRequest.fromLocation.lat
  let lon = searchRequest.fromLocation.lon

  -- Add to GSI with format: estimateId:validTill:numSeats (updated format)
  Redis.geoAdd gsiKey [(lon, lat, memberKey)]

-- Note: GSI members don't support TTL directly. Cleanup will be handled by:
-- 1. Validation during candidate filtering (expired candidates rejected)
-- 2. Periodic cleanup job to remove expired GSI entries
-- TODO: Implement periodic GSI cleanup job in Chunk 9 (async pooling cron)

-- Chunk 5: Sync rider pooling handler with GSI queries and filtering cascade
handleSyncRiderPooling ::
  (ServiceFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  m (Maybe (Id DSE.SharedEntity))
handleSyncRiderPooling config searchRequest estimate numSeats = do
  logInfo $ "Starting Chunk 5 rider pooling for estimate: " <> estimate.id.getId <> " with " <> show numSeats <> " seats"

  let pickupLocation = getCoordinates searchRequest.fromLocation
      searchRadius = config.pickupLocationSearchRadius -- Non-Maybe field in SharedRideConfigs
      gsiKey = "ShareRideCustomerLoc"

  -- Step 1: Query GSI for nearby riders within radius
  -- Use proper Hedis.GeoFrom and Hedis.GeoBy parameters as per FRFS implementation
  gsiMembers <-
    Redis.geoSearchDecoded
      gsiKey
      (Hedis.FromLonLat pickupLocation.lon pickupLocation.lat)
      (Hedis.ByRadius (toDouble searchRadius / 1000) "km") -- Convert meters to km
  logInfo $ "Found " <> show (length gsiMembers) <> " potential candidates in GSI"

  now <- getCurrentTime

  -- Step 2: Parse GSI members (fixed lambda syntax)
  let parsedMembers = mapMaybe (\(memberText, coords) -> parseGSIMember memberText now coords) gsiMembers
  logInfo $ "Successfully parsed " <> show (length parsedMembers) <> " GSI members"

  -- Step 3: Apply filtering cascade to all candidates
  validCandidates <- filterM (isValidCandidate config searchRequest estimate numSeats pickupLocation now) parsedMembers
  logInfo $ "After filtering cascade: " <> show (length validCandidates) <> " valid candidates"

  case validCandidates of
    [] -> do
      logInfo "No valid candidates found for pooling"
      return Nothing
    _ -> do
      -- Step 4: Chunk 7 final capacity validation and grouping
      logInfo $ "Starting Chunk 7 final validation for " <> show (length validCandidates) <> " valid candidates"
      finalGrouping <- validateFinalCapacityAndGrouping config searchRequest estimate numSeats validCandidates

      case finalGrouping of
        Nothing -> do
          logInfo "No valid grouping found in final capacity validation"
          return Nothing
        Just finalCandidates -> do
          -- Step 5: Create SharedEntity for successful match
          -- Acquire locks on final matched estimateIds to prevent concurrent grouping
          logInfo $ "Acquiring locks for " <> show (length finalCandidates) <> " final candidate estimateIds"
          finalLockResults <- mapM (acquireLockOnEstimate . (.gsiEstimateId)) finalCandidates
          let successfullyLockedEstimateIds = [candidate.gsiEstimateId | (candidate, True) <- zip finalCandidates finalLockResults]

          if length successfullyLockedEstimateIds == length finalCandidates
            then do
              logInfo $ "Creating SharedEntity with " <> show (length finalCandidates) <> " final candidates"
              sharedEntityId <- createSharedEntityForFinalMatch config searchRequest estimate finalCandidates numSeats
              logInfo $ "Chunk 7 complete. SharedEntity created: " <> sharedEntityId.getId
              -- Note: Locks remain held for these estimateIds to prevent them from being grouped elsewhere
              return (Just sharedEntityId)
            else do
              -- Some locks failed, release acquired locks and return Nothing
              logInfo "Lock acquisition failed for some candidates, releasing acquired locks"
              mapM_ (releaseLockOnEstimate) successfullyLockedEstimateIds
              return Nothing
  where
    toDouble :: Meters -> Double
    toDouble (Meters m) = fromIntegral m

-- Modular validation approach replacing nested if-else statements
isValidCandidate ::
  (ServiceFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  LatLong ->
  UTCTime ->
  GSIMember ->
  m Bool
isValidCandidate config searchRequest estimate numSeats pickupLocation now candidate = do
  -- Get candidate estimate and search request once with proper error handling
  candidateEstimate <- QEstimate.findById candidate.gsiEstimateId >>= fromMaybeM (EstimateDoesNotExist candidate.gsiEstimateId.getId)
  candidateSearchRequest <- QSearchRequest.findById candidateEstimate.requestId >>= fromMaybeM (SearchRequestDoesNotExist candidateEstimate.requestId.getId)

  -- Run validation steps with early exit on first failure (performance optimization)
  lockStep <- checkLockStatus candidate
  if not lockStep.isValid
    then do
      logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed lock check: " <> fromMaybe "unknown" lockStep.reason
      return False
    else do
      expiryStep <- checkExpiryTime candidate now
      if not expiryStep.isValid
        then do
          logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed expiry check: " <> fromMaybe "unknown" expiryStep.reason
          return False
        else do
          vehicleStep <- checkVehicleCompatibility searchRequest candidateSearchRequest
          if not vehicleStep.isValid
            then do
              logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed vehicle compatibility check: " <> fromMaybe "unknown" vehicleStep.reason
              return False
            else do
              seatStep <- checkSeatAvailability candidate candidateEstimate numSeats
              if not seatStep.isValid
                then do
                  logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed seat availability check: " <> fromMaybe "unknown" seatStep.reason
                  return False
                else do
                  dropoffStep <- checkDropoffDistanceCompatibility config searchRequest candidateSearchRequest candidateEstimate
                  if not dropoffStep.isValid
                    then do
                      logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed dropoff distance check: " <> fromMaybe "unknown" dropoffStep.reason
                      return False
                    else do
                      -- Check if route overlap analysis is needed based on drop distance deviation
                      shouldCheckRouteOverlap <- checkIfRouteOverlapNeeded config searchRequest candidateSearchRequest
                      if shouldCheckRouteOverlap
                        then do
                          routeOverlapStep <- checkRouteOverlapCompatibility config searchRequest candidateSearchRequest candidateEstimate
                          if routeOverlapStep.isValid
                            then do
                              logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " passed all validation steps including route overlap"
                              return True
                            else do
                              logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " failed route overlap check: " <> fromMaybe "unknown" routeOverlapStep.reason
                              return False
                        else do
                          logInfo $ "Candidate " <> candidate.gsiEstimateId.getId <> " passed all validation steps (route overlap not needed)"
                          return True

-- Check vehicle category compatibility between current rider and candidate
checkVehicleCompatibility ::
  DSearchReq.SearchRequest ->
  DSearchReq.SearchRequest ->
  m ValidationStep
checkVehicleCompatibility searchRequest candidateSearchRequest = do
  let currentVehicle = searchRequest.vehicleCategory
      candidateVehicle = candidateSearchRequest.vehicleCategory
      isCompatible = currentVehicle == candidateVehicle

  return $
    ValidationStep
      { stepName = "VehicleCompatibility",
        isValid = isCompatible,
        reason =
          if not isCompatible
            then Just ("vehicle mismatch: current=" <> show currentVehicle <> ", candidate=" <> show candidateVehicle)
            else Nothing
      }

-- Check if route overlap analysis is needed based on drop distance deviation (x3 threshold)
checkIfRouteOverlapNeeded ::
  (ServiceFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DSearchReq.SearchRequest ->
  m Bool
checkIfRouteOverlapNeeded config searchRequest candidateSearchRequest = do
  case (searchRequest.toLocation, candidateSearchRequest.toLocation) of
    (Just currentDest, Just candidateDest) -> do
      -- Calculate distance between destinations
      let dropDistance =
            calculateDistanceFromCoords
              (currentDest.lat, currentDest.lon)
              (candidateDest.lat, candidateDest.lon)

      -- Get minimum ride distance from SearchRequest.distance field if available
      let currentRideDistance = fromMaybe (Distance 0 Meter) searchRequest.distance
          candidateRideDistance = fromMaybe (Distance 0 Meter) candidateSearchRequest.distance
          minRideDistance = min currentRideDistance candidateRideDistance

      if minRideDistance.getMeters > 0
        then do
          let deviationRatio = (dropDistance.getMeters / minRideDistance.getMeters) * 100
              threshold = config.routeMatchingThreshold -- x3 parameter
          logInfo $ "Drop distance deviation: " <> show deviationRatio <> "% vs threshold: " <> show threshold <> "%"
          return (deviationRatio > threshold)
        else do
          -- If no distance data available, default to checking route overlap
          return True
    _ -> return False -- No destinations, no need for route overlap check

-- Step 1: Check if estimate is locked using Redis.safeGet
checkLockStatus :: (CacheFlow m r) => GSIMember -> m ValidationStep
checkLockStatus candidate = do
  let lockKey = generateLockKey candidate.gsiEstimateId
  lockValue <- Redis.safeGet lockKey
  return $
    ValidationStep
      { stepName = "LockCheck",
        isValid = isNothing lockValue,
        reason = if isJust lockValue then Just "estimate is locked" else Nothing
      }

-- Step 2: Check expiry time
checkExpiryTime :: GSIMember -> UTCTime -> m ValidationStep
checkExpiryTime candidate now = do
  let isExpired = candidate.gsiValidTill < now
  return $
    ValidationStep
      { stepName = "ExpiryCheck",
        isValid = not isExpired,
        reason = if isExpired then Just "estimate has expired" else Nothing
      }

-- Step 3: Check seat availability
checkSeatAvailability :: GSIMember -> DEstimate.Estimate -> Int -> m ValidationStep
checkSeatAvailability candidate candidateEstimate requestedSeats = do
  let maxSeats = getVehicleCapacity candidateEstimate.vehicleServiceTierType
      totalSeatsNeeded = candidate.gsiNumSeats + requestedSeats
      hasEnoughSeats = totalSeatsNeeded <= maxSeats

  return $
    ValidationStep
      { stepName = "SeatAvailability",
        isValid = hasEnoughSeats,
        reason =
          if not hasEnoughSeats
            then Just ("insufficient seats: has " <> show candidate.gsiNumSeats <> ", requesting " <> show requestedSeats <> ", max " <> show maxSeats)
            else Nothing
      }

-- Step 4: Enhanced dropoff distance compatibility check with staged validation
checkDropoffDistanceCompatibility ::
  (ServiceFlow m r, EsqDBFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  m ValidationStep
checkDropoffDistanceCompatibility config searchRequest candidateSearchRequest candidateEstimate = do
  case searchRequest.toLocation of
    Nothing -> return $ ValidationStep "DropoffCheck" True Nothing
    Just currDestination -> do
      case candidateSearchRequest.toLocation of
        Nothing -> return $ ValidationStep "DropoffCheck" True Nothing
        Just candidateDestination -> do
          -- First: Check if destinations are within config radius (straight-line check)
          let straightLineDistance =
                calculateDistanceFromCoords
                  (currDestination.lat, currDestination.lon)
                  (candidateDestination.lat, candidateDestination.lon)
              radiusThreshold = Distance (toHighPrecDistance config.dropLocationSearchRadius) Meter

          if straightLineDistance > radiusThreshold
            then
              return $
                ValidationStep
                  { stepName = "DropoffCheck",
                    isValid = False,
                    reason = Just ("destinations outside radius: " <> show straightLineDistance <> " > " <> show radiusThreshold)
                  }
            else do
              -- Implementation of the 3 required distance checks:
              -- 1. Straight-line distance between destinations (already checked above)
              -- 2. Pickup location compatibility using SearchRequest.distance if available
              let currRideDistance = fromMaybe (Distance 0 Meter) searchRequest.distance
                  candidateRideDistance = fromMaybe (Distance 0 Meter) candidateSearchRequest.distance
                  pickupThreshold = Distance (toHighPrecDistance config.actualPickupDistanceThreshold) Meter

              -- Check 2: Pickup distance compatibility
              pickupCompatible <-
                if currRideDistance.getMeters > 0 && candidateRideDistance.getMeters > 0
                  then do
                    let rideLengthDiff = abs (currRideDistance.getMeters - candidateRideDistance.getMeters)
                    return $ rideLengthDiff <= pickupThreshold.getMeters
                  else do
                    -- Fallback: calculate actual pickup distance if SearchRequest.distance not available
                    pickupDistance <-
                      getActualDistance
                        searchRequest.merchantId
                        searchRequest.merchantOperatingCityId
                        searchRequest.fromLocation
                        candidateSearchRequest.fromLocation
                    return $ pickupDistance <= pickupThreshold

              if not pickupCompatible
                then
                  return $
                    ValidationStep
                      { stepName = "DropoffCheck",
                        isValid = False,
                        reason = Just ("pickup locations incompatible")
                      }
                else do
                  -- Check 3: Drop destination distance using config threshold
                  let dropThreshold = Distance (toHighPrecDistance config.actualDropDistanceThreshold) Meter
                      isDropCompatible = straightLineDistance <= dropThreshold

                  return $
                    ValidationStep
                      { stepName = "DropoffCheck",
                        isValid = isDropCompatible,
                        reason =
                          if not isDropCompatible
                            then Just ("drop destinations too far: " <> show straightLineDistance <> " > " <> show dropThreshold)
                            else Nothing
                      }

-- VaibhavD : Re-evaluate this, should be from config?
-- TODO: Move these hardcoded vehicle capacities to SharedRideConfigs
-- Get vehicle capacity based on service tier
getVehicleCapacity :: ServiceTierType -> Int
getVehicleCapacity serviceTier =
  case serviceTier of
    AUTO_RICKSHAW -> 1
    HATCHBACK -> 2
    SEDAN -> 2
    SUV -> 2
    _ -> 2 -- Default to 2 for unknown types

-- Step 5: Advanced route overlap compatibility check using geo-hashing
checkRouteOverlapCompatibility ::
  (ServiceFlow m r, EsqDBFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  m ValidationStep
checkRouteOverlapCompatibility config searchRequest candidateSearchRequest candidateEstimate = do
  case (searchRequest.toLocation, candidateSearchRequest.toLocation) of
    (Just currDestination, Just candidateDestination) -> do
      -- Get route hashes from cached route data (Chunk 1 implementation)
      currentRouteHashes <- getRouteHashesFromCache searchRequest.id
      candidateRouteHashes <- getRouteHashesFromCache candidateSearchRequest.id

      if Set.null currentRouteHashes || Set.null candidateRouteHashes
        then
          return $
            ValidationStep
              { stepName = "RouteOverlapCheck",
                isValid = False,
                reason = Just "route data unavailable for overlap analysis"
              }
        else do
          -- Calculate overlap percentage
          let intersectionSize = Set.size $ Set.intersection currentRouteHashes candidateRouteHashes
              totalPoints = Set.size candidateRouteHashes
              overlapPercentage =
                if totalPoints > 0
                  then (fromIntegral intersectionSize / fromIntegral totalPoints) * 100
                  else 0
              -- Use config threshold (default 30% for route overlap)
              thresholdPercentage = fromMaybe 30.0 config.routeOverlapThreshold

          let isCompatible = overlapPercentage >= thresholdPercentage

          return $
            ValidationStep
              { stepName = "RouteOverlapCheck",
                isValid = isCompatible,
                reason =
                  if not isCompatible
                    then Just ("insufficient route overlap: " <> show overlapPercentage <> "% < " <> show thresholdPercentage <> "%")
                    else Nothing
              }
    _ -> return $ ValidationStep "RouteOverlapCheck" True Nothing

-- Get route hashes from cached route data (leveraging Chunk 1 route caching)
getRouteHashesFromCache ::
  (CacheFlow m r) =>
  Id DSearchReq.SearchRequest ->
  m (Set.Set Text)
getRouteHashesFromCache searchRequestId = do
  let routeCacheKey = "route_cache:" <> searchRequestId.getId

  -- Try to get route data from cache (as implemented in Chunk 1)
  -- The cached data should be GetRoutesResp which contains routes with points
  mbRouteResponse <- Redis.safeGet routeCacheKey
  case mbRouteResponse of
    Just routeResponse -> do
      -- Extract route points from the first route in response
      case extractRoutePointsFromMapsResponse routeResponse of
        Just points -> do
          let geoHashes = mapMaybe (geoHashPrecision9) points
          return $ Set.fromList geoHashes
        Nothing -> return Set.empty
    Nothing -> do
      -- No cached route found
      logInfo $ "No cached route found for searchRequestId: " <> searchRequestId.getId
      return Set.empty
  where
    -- TODO: Move geohash precision to config (currently hardcoded to 9)
    geoHashPrecision9 :: LatLong -> Maybe Text
    geoHashPrecision9 (LatLong lat lon) =
      case Geohash.encode 9 (lat, lon) of
        Just hash -> Just (T.pack hash)
        Nothing -> Nothing

-- Extract route points from Maps GetRoutesResp (first route's points)
extractRoutePointsFromMapsResponse :: GetRoutesResp -> Maybe [LatLong]
extractRoutePointsFromMapsResponse routeResponse = do
  -- GetRoutesResp contains routes, get first route's points
  firstRoute <- listToMaybe routeResponse.routes
  return firstRoute.points

-- Chunk 7: Generic final capacity validation for any vehicle category
validateFinalCapacityAndGrouping ::
  (ServiceFlow m r, EsqDBFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  Int ->
  [GSIMember] ->
  m (Maybe [GSIMember])
validateFinalCapacityAndGrouping config searchRequest estimate numSeats validCandidates = do
  let vehicleCategory = searchRequest.vehicleCategory
      maxCapacity = getVehicleCapacity estimate.vehicleServiceTierType
      availableSeats = maxCapacity - numSeats

  logInfo $ "Final capacity validation for " <> show vehicleCategory <> " with max capacity " <> show maxCapacity <> " and " <> show availableSeats <> " available seats"

  if availableSeats <= 0
    then do
      logInfo "No available seats for sharing"
      return Nothing
    else do
      -- Find optimal combination of candidates that fit in available capacity
      optimalGrouping <- findOptimalGrouping availableSeats validCandidates
      case optimalGrouping of
        Just group -> do
          logInfo $ "Found optimal grouping with " <> show (length group) <> " candidates for " <> show vehicleCategory
          return $ Just group
        Nothing -> do
          logInfo $ "No valid grouping found for " <> show vehicleCategory <> " within capacity constraints"
          return Nothing

-- Find optimal combination of candidates for any vehicle type
-- For sync pooling: prioritize single candidate matching for performance and simplicity
-- For async pooling: use greedy algorithm for complex combinations
findOptimalGrouping ::
  (MonadFlow m) =>
  Int -> -- Available seats
  [GSIMember] ->
  m (Maybe [GSIMember])
findOptimalGrouping availableSeats candidates = do
  if null candidates
    then return Nothing
    else do
      -- Sort candidates by seat count (prefer fewer seats for efficient packing)
      let sortedCandidates = sortBy (comparing (.gsiNumSeats)) candidates

      -- For sync pooling: focus on single rider matching (performance + simplicity)
      -- This reduces complexity and latency for real-time pooling requests
      singleResult <- trySingleCandidate availableSeats sortedCandidates
      case singleResult of
        Just result -> do
          logInfo "Sync pooling: Single candidate match found"
          return $ Just result
        Nothing -> do
          logInfo "Sync pooling: No single candidate fit, skipping complex combinations for performance"
          -- For sync pooling, we skip complex combinations to maintain low latency
          -- Complex multi-candidate grouping is handled by async pooling (Chunk 9 cron)
          return Nothing

-- Strategy 1: Try single candidate (most efficient)
trySingleCandidate ::
  (MonadFlow m) =>
  Int ->
  [GSIMember] ->
  m (Maybe [GSIMember])
trySingleCandidate availableSeats candidates = do
  let validSingle = find (\candidate -> candidate.gsiNumSeats <= availableSeats) candidates
  case validSingle of
    Just candidate -> do
      logInfo $ "Single candidate strategy: " <> candidate.gsiEstimateId.getId <> " with " <> show candidate.gsiNumSeats <> " seats"
      return $ Just [candidate]
    Nothing -> return Nothing

-- Strategy 2: Try pair combination
tryPairCombination ::
  (MonadFlow m) =>
  Int ->
  [GSIMember] ->
  m (Maybe [GSIMember])
tryPairCombination availableSeats candidates = do
  let pairs =
        [ (c1, c2) | c1 <- candidates, c2 <- candidates, c1.gsiEstimateId /= c2.gsiEstimateId, c1.gsiNumSeats + c2.gsiNumSeats <= availableSeats
        ]
  case listToMaybe pairs of
    Just (c1, c2) -> do
      logInfo $ "Pair combination strategy: " <> c1.gsiEstimateId.getId <> " + " <> c2.gsiEstimateId.getId <> " = " <> show (c1.gsiNumSeats + c2.gsiNumSeats) <> " seats"
      return $ Just [c1, c2]
    Nothing -> return Nothing

-- Strategy 3: Try multiple combination (greedy algorithm for 3+ candidates)
tryMultipleCombination ::
  (MonadFlow m) =>
  Int ->
  [GSIMember] ->
  m (Maybe [GSIMember])
tryMultipleCombination availableSeats candidates = do
  if length candidates < 3
    then return Nothing
    else do
      -- Use greedy algorithm: keep adding candidates until capacity is reached
      optimalCombination <- greedyPackingAlgorithm availableSeats candidates []
      case optimalCombination of
        [] -> return Nothing
        group -> do
          let totalSeats = sum $ map (.gsiNumSeats) group
          logInfo $ "Multiple combination strategy: " <> show (length group) <> " candidates with " <> show totalSeats <> " total seats"
          return $ Just group

-- Greedy packing algorithm for optimal seat utilization
greedyPackingAlgorithm ::
  (MonadFlow m) =>
  Int -> -- Remaining capacity
  [GSIMember] -> -- Available candidates
  [GSIMember] -> -- Current selection
  m [GSIMember]
greedyPackingAlgorithm remainingCapacity availableCandidates currentSelection = do
  if remainingCapacity <= 0 || null availableCandidates
    then return currentSelection
    else do
      -- Find the best-fitting candidate (largest that still fits)
      let fittingCandidates = filter (\c -> c.gsiNumSeats <= remainingCapacity) availableCandidates
      case fittingCandidates of
        [] -> return currentSelection
        _ -> do
          let bestCandidate = maximumBy (comparing (.gsiNumSeats)) fittingCandidates
              newRemaining = remainingCapacity - bestCandidate.gsiNumSeats
              newAvailable = filter (\c -> c.gsiEstimateId /= bestCandidate.gsiEstimateId) availableCandidates
              newSelection = bestCandidate : currentSelection
          greedyPackingAlgorithm newRemaining newAvailable newSelection

-- Acquire lock on specific estimateId to prevent concurrent grouping
acquireLockOnEstimate :: (CacheFlow m r) => Id DEstimate.Estimate -> m Bool
acquireLockOnEstimate estimateId = do
  let lockKey = generateLockKey estimateId
      lockTtl = 300 -- TODO: Move to config - 5 minutes lock TTL
  lockAcquired <- Redis.setNX lockKey "final_locked" lockTtl
  if lockAcquired
    then do
      logInfo $ "Final lock acquired for estimateId: " <> estimateId.getId
      return True
    else do
      logInfo $ "Final lock acquisition failed for estimateId: " <> estimateId.getId
      return False

-- Release lock on specific estimateId
releaseLockOnEstimate :: (CacheFlow m r) => Id DEstimate.Estimate -> m ()
releaseLockOnEstimate estimateId = do
  let lockKey = generateLockKey estimateId
  Redis.del [lockKey]
  logInfo $ "Released lock for estimateId: " <> estimateId.getId

-- Enhanced SharedEntity creation for final matches with multiple candidates
createSharedEntityForFinalMatch ::
  (MonadFlow m, EsqDBFlow m r) =>
  SharedRideConfigs ->
  DSearchReq.SearchRequest ->
  DEstimate.Estimate ->
  [GSIMember] ->
  Int ->
  m (Id DSE.SharedEntity)
createSharedEntityForFinalMatch config searchRequest estimate finalCandidates numSeats = do
  sharedEntityId <- generateGUID
  now <- getCurrentTime

  -- Calculate total seats for all participants
  let candidateSeats = sum $ map (.gsiNumSeats) finalCandidates
      totalSeats = candidateSeats + numSeats
      validTill = addUTCTime (fromIntegral config.searchRequestExpirySeconds.getSeconds) now

      -- Create estimate IDs list including current customer + all candidates
      allEstimateIds = estimate.id : map (.gsiEstimateId) finalCandidates
      trackedEstimateIds = map mkTrackedEntity allEstimateIds

      sharedEntity =
        DSE.SharedEntity
          { id = sharedEntityId,
            merchantId = searchRequest.merchantId,
            merchantOperatingCityId = searchRequest.merchantOperatingCityId,
            entityType = DSE.OVERLAPPING,
            status = DSE.SEARCHING,
            totalSeats = totalSeats,
            validTill = validTill,
            tripCategory = RideShare,
            vehicleCategory = searchRequest.vehicleCategory,
            estimateIds = Just trackedEstimateIds,
            searchRequestIds = Just [mkTrackedEntity searchRequest.id],
            bookingIds = Nothing,
            rideIds = Nothing,
            driverId = Nothing,
            pooledUsingCustomer = Just searchRequest.riderId,
            pairingTime = Just now,
            transactionId = Nothing,
            waypoints = A.object [], -- Will be populated when route optimization is implemented
            createdAt = now,
            updatedAt = now
          }

  QSharedEntity.create sharedEntity
  logInfo $ "Created SharedEntity " <> sharedEntityId.getId <> " with " <> show (length finalCandidates + 1) <> " customers and " <> show totalSeats <> " total seats"
  return sharedEntityId
  where
    mkTrackedEntity :: Id a -> DSE.TrackedEntity
    mkTrackedEntity entityId = DSE.TrackedEntity entityId.getId now
