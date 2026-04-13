{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
  You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.StateEntryPermitDetector where

import Data.List (nub, nubBy, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Domain.Types.BoundingBoxPoints (BoundingBoxPoints (BoundingBoxPoints))
import qualified Domain.Types.Geometry as DGeo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.StateEntryPermitCharges
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import Storage.Queries.Geometry (findGeometriesContainingGps, findGeometryById)
import Storage.Queries.StateEntryPermitChargesExtra (findAllStateEntryPermitCharges)

-- | Convert stored bbox points (topLeft, topRight, bottomRight, bottomLeft) into four sides.
boundingBoxPointsToLineSegments :: BoundingBoxPoints -> Maybe [LineSegment]
boundingBoxPointsToLineSegments (BoundingBoxPoints pts) = case pts of
  (p0 : p1 : p2 : p3 : _) ->
    Just
      [ LineSegment p0 p1,
        LineSegment p1 p2,
        LineSegment p2 p3,
        LineSegment p3 p0
      ]
  _ -> Nothing

-- | Four sides of a Kernel BoundingBox (topLeft, topRight, bottomRight, bottomLeft).
boundingBoxToLineSegments :: BoundingBox -> [LineSegment]
boundingBoxToLineSegments box =
  [ LineSegment (topLeft box) (topRight box),
    LineSegment (topRight box) (bottomRight box),
    LineSegment (bottomRight box) (bottomLeft box),
    LineSegment (bottomLeft box) (topLeft box)
  ]

-- | True if route bounding box (RBB) intersects geometry bounding box (SBB); used to skip candidates that cannot be on route.
-- When geometry has no bbox (Nothing), we return False so candidates with null bbox are dropped (many geometry rows have bbox null).
-- Handles two cases: edge-edge intersection, and RBB entirely inside SBB (route fully within a state boundary).
-- Returns False with a warning when the stored bbox has fewer than 4 points (malformed data).
boundingBoxesIntersect :: (MonadFlow m) => BoundingBox -> Maybe BoundingBoxPoints -> m Bool
boundingBoxesIntersect _ Nothing = do
  logDebug "SEPC: boundingBoxesIntersect — geometry has no bbox (null), skipping"
  pure False
boundingBoxesIntersect rbb (Just sbbPoints) =
  case boundingBoxPointsToLineSegments sbbPoints of
    Nothing -> do
      logWarning $ "SEPC: Geometry has malformed bbox (fewer than 4 points): " <> show sbbPoints <> " — skipping candidate"
      pure False
    Just sbbSides -> do
      let rbbSides = boundingBoxToLineSegments rbb
          edgesIntersect = any (\rbbSide -> any (doIntersect rbbSide) sbbSides) rbbSides
          rbbCorners = [topLeft rbb, topRight rbb, bottomRight rbb, bottomLeft rbb]
          BoundingBoxPoints sbbCorners = sbbPoints
          sbb = getBoundingBox sbbCorners
          rbbInsideSbb = not (null sbbCorners) && any (`pointWithinBoundingBox` sbb) rbbCorners
          result = edgesIntersect || rbbInsideSbb
      logDebug $
        "SEPC: boundingBoxesIntersect"
          <> " | rbbSides: " <> show rbbSides
          <> " | sbbSides: " <> show sbbSides
          <> " | rbbCorners: " <> show rbbCorners
          <> " | sbbCorners: " <> show sbbCorners
          <> " | edgesIntersect: " <> show edgesIntersect
          <> " | rbbInsideSbb: " <> show rbbInsideSbb
          <> " | result: " <> show result
      pure result

-- | Cache key version for in-pod SEPC+geometry cache. TTL is 12 hours; bump this when geom data changes.
sepcGeomCacheKeyVersion :: Text
sepcGeomCacheKeyVersion = "geom:v1"

-- | Load all SEPC entries and their geometry rows, using in-pod cache so we hit DB at most once per TTL per pod.
loadCachedSEPCEntriesWithGeometries ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasInMemEnv r) =>
  m ([StateEntryPermitCharges], Map.Map (Id DGeo.Geometry) DGeo.Geometry)
loadCachedSEPCEntriesWithGeometries = do
  logDebug "SEPC: Loading cached SEPC entries with geometries from in-mem cache"
  IM.withInMemCache ["CACHED_SEPC_GEOMS", sepcGeomCacheKeyVersion] 43200 $ do
    allSEPCEntries <- findAllStateEntryPermitCharges
    logInfo $ "SEPC: Loaded " <> show (length allSEPCEntries) <> " SEPC entries from DB"
    let uniqueGeomIds = nub $ map (.geomId) allSEPCEntries
    logDebug $ "SEPC: Unique geometry IDs to fetch: " <> show (length uniqueGeomIds)
    geomsMaybe <- mapM findGeometryById uniqueGeomIds
    let geometryByGeomId = Map.fromList [(g.id, g) | Just g <- geomsMaybe]
    logInfo $ "SEPC: Loaded " <> show (Map.size geometryByGeomId) <> " geometries for SEPC entries"
    pure (allSEPCEntries, geometryByGeomId :: Map.Map (Id DGeo.Geometry) DGeo.Geometry)

-- | Derive the source state from the first route point (which geometries contain it), or use fallback if none.
getSourceStateFromFirstPoint ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  LatLong ->
  Context.IndianState ->
  m Context.IndianState
getSourceStateFromFirstPoint firstPoint fallbackState = do
  logDebug $ "SEPC: Finding source state from first point: " <> show firstPoint
  geoms <- findGeometriesContainingGps firstPoint
  let detectedState = DGeo.state <$> listToMaybe geoms
  logInfo $ "SEPC: Source point detected in state: " <> show detectedState <> ", fallback state: " <> show fallbackState
  pure $ fromMaybe fallbackState detectedState

-- | Allowed destination states for a given source state (from MerchantState config, or just source if not configured).
getAllowedDestinationStates ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Context.IndianState ->
  m [Context.IndianState]
getAllowedDestinationStates merchantId sourceState = do
  logDebug $ "SEPC: Getting allowed destination states for merchantId: " <> getId merchantId <> ", sourceState: " <> show sourceState
  mbMerchantState <- CQMS.findByMerchantIdAndState merchantId sourceState
  let allowedStates = maybe [sourceState] (.allowedDestinationStates) mbMerchantState
  logInfo $ "SEPC: Allowed destination states from " <> show sourceState <> ": " <> show allowedStates
  pure allowedStates

-- | Keep only SEPC entries whose geometry's state is in the allowed destination states list.
filterSEPCsByAllowedState ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) DGeo.Geometry ->
  [Context.IndianState] ->
  [StateEntryPermitCharges]
filterSEPCsByAllowedState allSEPCEntries geometryByGeomId allowedStates =
  let result = filter hasAllowedState allSEPCEntries
   in result
  where
    hasAllowedState sepcRow =
      case Map.lookup sepcRow.geomId geometryByGeomId of
        Nothing -> False
        Just geomRow -> geomRow.state `elem` allowedStates

-- | Keep only SEPC entries whose geometry bounding box intersects the route bounding box (drops null bbox / no intersection).
filterSEPCsByRouteBboxIntersection ::
  (MonadFlow m) =>
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) DGeo.Geometry ->
  BoundingBox ->
  m [StateEntryPermitCharges]
filterSEPCsByRouteBboxIntersection candidateSEPCs geometryByGeomId routeBoundingBox = do
  logDebug $ "SEPC: filterSEPCsByRouteBboxIntersection — checking " <> show (length candidateSEPCs) <> " candidates against route bbox: " <> show routeBoundingBox
  results <- filterM intersectsRouteBbox candidateSEPCs
  logDebug $ "SEPC: filterSEPCsByRouteBboxIntersection — " <> show (length results) <> " candidates passed bbox filter"
  pure results
  where
    intersectsRouteBbox sepcRow =
      case Map.lookup sepcRow.geomId geometryByGeomId of
        Nothing -> do
          logDebug $ "SEPC: geomId " <> getId sepcRow.geomId <> " not found in geometry map — skipping"
          pure False
        Just geomRow -> do
          logDebug $ "SEPC: checking geomId " <> getId sepcRow.geomId <> " bbox: " <> show geomRow.bbox
          boundingBoxesIntersect routeBoundingBox geomRow.bbox

-- | Build a map from state to SEPC for eligible candidates (one SEPC per state).
buildStateToSEPCMap ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) DGeo.Geometry ->
  Map.Map Context.IndianState StateEntryPermitCharges
buildStateToSEPCMap eligibleCandidates geometryByGeomId =
  Map.fromList
    [(geomRow.state, sepcRow) | sepcRow <- eligibleCandidates, Just geomRow <- [Map.lookup sepcRow.geomId geometryByGeomId]]

-- | Walk route segments, detect state transitions via point-in-polygon, and accumulate charges (at most once per SEPC id).
computeChargesAlongRouteSegments ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [(LatLong, LatLong)] ->
  Set.Set Text ->
  Map.Map Context.IndianState StateEntryPermitCharges ->
  m (HighPrecMoney, [Text], [Text])
computeChargesAlongRouteSegments routeSegments candidateGeomIds stateToSEPC = do
  (total, names, ids, _) <-
    foldM
      ( \(accTotal, accNames, accIds, chargedSet) (p1, p2) -> do
          geoms1 <- findGeometriesContainingGps p1
          geoms2 <- findGeometriesContainingGps p2

          let inCandidate g = getId g.id `Set.member` candidateGeomIds
              geoms1' = filter inCandidate geoms1
              geoms2' = filter inCandidate geoms2
              states1 = nub $ map (.state) geoms1'
              states2 = nub $ map (.state) geoms2'
              destinationStatesEntered = states2 \\ states1
              sepcsForDestinationStatesEntered = mapMaybe (`Map.lookup` stateToSEPC) destinationStatesEntered

              toCharge =
                filter
                  (\s -> getId s.id `Set.notMember` chargedSet)
                  (nubBy (\a b -> getId a.id == getId b.id) sepcsForDestinationStatesEntered)

              newCharged = chargedSet <> Set.fromList (map (getId . (.id)) toCharge)
              addTotal = sum $ map (.amount) toCharge
              addNames = map (fromMaybe "" . (.name)) toCharge
              addIds = map (getId . (.id)) toCharge

          pure (accTotal + addTotal, accNames <> addNames, accIds <> addIds, newCharged)
      )
      (0, [], [], Set.empty)
      routeSegments
  pure (total, names, ids)

-- | Returns state entry permit charges (total, names, ids) for the given route, or Nothing if none apply.
-- Uses: source state from first point, allowed states from config, cached SEPC+geometries, bbox filter, then segment-wise state transition charges.
getStateEntryPermitInfoOnRoute ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasInMemEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DP.Driver) ->
  RoutePoints ->
  m (Maybe (HighPrecMoney, [Text], [Text]))
getStateEntryPermitInfoOnRoute merchantOperatingCityId mbDriverId route = do
  logInfo $ "SEPC: Starting SEPC detection for merchantOpCityId: " <> getId merchantOperatingCityId <> ", route points: " <> show (length route) <> ", driverId: " <> maybe "N/A" getId mbDriverId
  case route of
    [] -> do
      logDebug "SEPC: Empty route, no SEPC charges"
      pure Nothing
    [_] -> do
      logDebug "SEPC: Single point route, no SEPC charges"
      pure Nothing
    firstPoint : _ -> do
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (InternalError "MerchantOperatingCity not found")

      sourceState <- getSourceStateFromFirstPoint firstPoint merchantOpCity.state
      allowedStates <- getAllowedDestinationStates merchantOpCity.merchantId sourceState

      (allSEPCEntries, geometryByGeomId) <- loadCachedSEPCEntriesWithGeometries

      let candidateSEPCsWithState = filterSEPCsByAllowedState allSEPCEntries geometryByGeomId allowedStates
      logInfo $ "SEPC: After allowed-state filter: " <> show (length candidateSEPCsWithState) <> " candidates from " <> show (length allowedStates) <> " allowed states"

      if null candidateSEPCsWithState
        then do
          logInfo "SEPC: No candidates after allowed-state filter, returning Nothing"
          pure Nothing
        else do
          let routeBoundingBox = getBoundingBox route
          logDebug $ "SEPC: Route bounding box calculated"
          eligibleCandidates <-
            filterSEPCsByRouteBboxIntersection candidateSEPCsWithState geometryByGeomId routeBoundingBox

          logInfo $ "SEPC: After bbox intersection filter: " <> show (length eligibleCandidates) <> " eligible candidates"

          if null eligibleCandidates
            then do
              logInfo "SEPC: No candidates after bbox filter, returning Nothing"
              pure Nothing
            else do
              let candidateGeomIds = Set.fromList $ map (getId . (.geomId)) eligibleCandidates
              let stateToSEPC = buildStateToSEPCMap eligibleCandidates geometryByGeomId
              let routeSegments = zip route (tail route)
              logDebug $ "SEPC: Analyzing " <> show (length routeSegments) <> " route segments for state transitions"

              (total, names, ids) <-
                computeChargesAlongRouteSegments routeSegments candidateGeomIds stateToSEPC

              if total > 0 && not (null names)
                then do
                  logInfo $ "SEPC: Charges detected - total: " <> show total <> ", names: " <> show names <> ", ids: " <> show ids
                  pure $ Just (total, names, ids)
                else do
                  logInfo "SEPC: No charges detected after segment analysis"
                  pure Nothing

-- | At end ride: reconcile estimated vs detected SEPC IDs and log the diff for debugging.
-- Returns (pendingIds, extraIds) where:
--   pendingIds = in estimate but NOT detected (expected state crossings that were missed)
--   extraIds   = detected but NOT in estimate (unexpected extra state crossings)
-- No charge amounts are computed here; charge decisions use ride fields directly
-- (estimatedStateEntryPermitCharges / stateEntryPermitCharges set during ride).
-- No Redis pending key for SEPC (unlike toll).
checkAndValidatePendingStateEntryPermits ::
  (MonadFlow m) =>
  Id DRide.Ride ->
  Maybe [Text] -> -- estimatedIds (from booking)
  Maybe [Text] -> -- detectedIds (accumulated during ride)
  m ([Text], [Text])
checkAndValidatePendingStateEntryPermits rideId estimatedIds alreadyDetectedIds = do
  let estIds = fromMaybe [] estimatedIds
      detIds = fromMaybe [] alreadyDetectedIds
      pendingIds = filter (`notElem` detIds) estIds -- estimated but not detected
      extraIds = filter (`notElem` estIds) detIds -- detected but not in estimate
  case (estimatedIds, alreadyDetectedIds) of
    (Nothing, Just _) ->
      logWarning $
        "Detected state entry permit(s) but NO estimated IDs. NOT charging for safety. RideId: "
          <> rideId.getId
          <> " | Detected: "
          <> show detIds
    _ ->
      logInfo $
        "SEPC ID reconciliation. RideId: "
          <> rideId.getId
          <> " | Estimated: "
          <> show estIds
          <> " | Detected: "
          <> show detIds
          <> " | Pending (missed): "
          <> show pendingIds
          <> " | Extra (unexpected): "
          <> show extraIds
  pure (pendingIds, extraIds)
