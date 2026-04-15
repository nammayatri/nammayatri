{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
  You should have received a copy of the GNU Affero General Public License along with this program, if not, see <https://www.gnu.org/licenses/>.
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
import SharedLogic.GeoPolygon (MultiPolygon, parseGeoJSONRings, pointInMultiPolygon)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import Storage.Queries.GeometryGeom (findGeometriesByIds)
import Storage.Queries.StateEntryPermitChargesExtra (findAllStateEntryPermitCharges)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Cached value per geometry: the metadata row plus pre-parsed polygon rings.
-- Rings are parsed once at cache-load time and reused for all subsequent
-- point-in-polygon checks — no further DB or JSON work in the hot path.
data GeomWithRings = GeomWithRings
  { geometry :: DGeo.Geometry,
    -- | Parsed from geometry.geom (ST_AsGeoJSON output). Nothing when geom is
    -- null or unparseable; such geometries are skipped during detection.
    rings :: Maybe MultiPolygon
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Bounding-box helpers
-- ---------------------------------------------------------------------------

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

-- | True if route bounding box (RBB) intersects or contains the geometry bounding box (SBB).
-- Returns False (with a debug log) when:
--   - The geometry has no stored bbox (common — bbox backfill may not be complete).
--   - The stored bbox has fewer than 4 points (malformed data).
boundingBoxesIntersect :: (MonadFlow m) => BoundingBox -> Maybe BoundingBoxPoints -> m Bool
boundingBoxesIntersect _ Nothing = do
  logDebug "SEPC: boundingBoxesIntersect — geometry has no bbox (null), skipping"
  pure False
boundingBoxesIntersect rbb (Just sbbPoints) =
  case boundingBoxPointsToLineSegments sbbPoints of
    Nothing -> do
      logWarning $ "SEPC: geometry has malformed bbox (fewer than 4 points): " <> show sbbPoints <> " — skipping"
      pure False
    Just sbbSides -> do
      let rbbSides = boundingBoxToLineSegments rbb
          edgesIntersect = any (\rbbSide -> any (doIntersect rbbSide) sbbSides) rbbSides
          BoundingBoxPoints sbbCorners = sbbPoints
          sbb = getBoundingBox sbbCorners
          rbbInsideSbb = not (null sbbCorners) && any (`pointWithinBoundingBox` sbb) (corners rbb)
          result = edgesIntersect || rbbInsideSbb
      logDebug $
        "SEPC: boundingBoxesIntersect"
          <> " | edgesIntersect: " <> show edgesIntersect
          <> " | rbbInsideSbb: " <> show rbbInsideSbb
          <> " | result: " <> show result
      pure result
  where
    corners box = [topLeft box, topRight box, bottomRight box, bottomLeft box]

-- ---------------------------------------------------------------------------
-- In-pod geometry cache
-- ---------------------------------------------------------------------------

-- | Cache key version. Bump this string whenever state geometry data changes in
-- the DB so all pods pick up the new geometry on their next request.
sepcGeomCacheKeyVersion :: Text
sepcGeomCacheKeyVersion = "geom:v1"

-- | Load all SEPC entries and their geometries from the in-pod cache.
-- On a cache miss, fetches from DB in two queries:
--   1. findAllStateEntryPermitCharges — all SEPC rows
--   2. findGeometriesByIds            — batch fetch of all referenced geometry rows
--      (includes geom via ST_AsGeoJSON, unlike findGeometryById which sets geom = Nothing)
-- GeoJSON rings are parsed immediately and stored alongside the geometry so that
-- all subsequent point-in-polygon checks are pure in-memory operations.
loadCachedSEPCEntriesWithGeometries ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasInMemEnv r) =>
  m ([StateEntryPermitCharges], Map.Map (Id DGeo.Geometry) GeomWithRings)
loadCachedSEPCEntriesWithGeometries =
  IM.withInMemCache ["CACHED_SEPC_GEOMS", sepcGeomCacheKeyVersion] 43200 $ do
    allSEPCEntries <- findAllStateEntryPermitCharges
    logInfo $ "SEPC cache load: fetched " <> show (length allSEPCEntries) <> " SEPC entries from DB"

    let uniqueGeomIds = nub $ map (.geomId) allSEPCEntries
    logDebug $ "SEPC cache load: fetching " <> show (length uniqueGeomIds) <> " geometry rows (batch)"

    geoms <- findGeometriesByIds uniqueGeomIds
    logInfo $ "SEPC cache load: fetched " <> show (length geoms) <> " geometry rows from DB"

    let geomWithRingsMap = Map.fromList
          [ (g.id, GeomWithRings {geometry = g, rings = g.geom >>= parseGeoJSONRings})
          | g <- geoms
          ]

    -- Warn about geometries whose geom column is null or unparseable — they will
    -- be skipped during detection, which means those SEPC rows effectively do nothing.
    let unparseable = [getId g.id | g <- geoms, Kernel.Prelude.isNothing (g.geom >>= parseGeoJSONRings)]
    unless (null unparseable) $
      logWarning $ "SEPC cache load: " <> show (length unparseable) <> " geometry rows have null/unparseable geom and will be skipped: " <> show unparseable

    pure (allSEPCEntries, geomWithRingsMap)

-- ---------------------------------------------------------------------------
-- Source state detection (pure, uses cached rings)
-- ---------------------------------------------------------------------------

-- | Derive the source state from the first route point using in-memory rings.
-- Falls back to the provided fallback state when the point matches no geometry.
-- Pure — no DB calls; caller is responsible for logging the result.
getSourceStateFromFirstPoint ::
  LatLong ->
  Context.IndianState ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  Context.IndianState
getSourceStateFromFirstPoint firstPoint fallbackState geomWithRingsMap =
  let matchingStates =
        [ gwr.geometry.state
        | gwr <- Map.elems geomWithRingsMap,
          Just polygons <- [gwr.rings],
          pointInMultiPolygon firstPoint polygons
        ]
   in fromMaybe fallbackState (listToMaybe matchingStates)

-- ---------------------------------------------------------------------------
-- Candidate filtering
-- ---------------------------------------------------------------------------

-- | Keep only SEPC entries whose geometry state is in the allowed destination states.
filterSEPCsByAllowedState ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  [Context.IndianState] ->
  [StateEntryPermitCharges]
filterSEPCsByAllowedState allSEPCEntries geomWithRingsMap allowedStates =
  filter hasAllowedState allSEPCEntries
  where
    hasAllowedState sepcRow =
      case Map.lookup sepcRow.geomId geomWithRingsMap of
        Nothing -> False
        Just gwr -> gwr.geometry.state `elem` allowedStates

-- | Keep only SEPC entries whose geometry bbox intersects the route bbox.
-- Geometries with no bbox are dropped (returns False + debug log).
filterSEPCsByRouteBboxIntersection ::
  (MonadFlow m) =>
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  BoundingBox ->
  m [StateEntryPermitCharges]
filterSEPCsByRouteBboxIntersection candidateSEPCs geomWithRingsMap routeBoundingBox = do
  logDebug $ "SEPC: bbox filter — checking " <> show (length candidateSEPCs) <> " candidates against route bbox"
  results <- filterM intersectsRouteBbox candidateSEPCs
  logDebug $ "SEPC: bbox filter — " <> show (length results) <> " candidates passed"
  pure results
  where
    intersectsRouteBbox sepcRow =
      case Map.lookup sepcRow.geomId geomWithRingsMap of
        Nothing -> do
          logDebug $ "SEPC: geomId " <> getId sepcRow.geomId <> " not in cache — skipping"
          pure False
        Just gwr ->
          boundingBoxesIntersect routeBoundingBox gwr.geometry.bbox

-- | Build a map from state → SEPC for eligible candidates (one SEPC per state).
buildStateToSEPCMap ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  Map.Map Context.IndianState StateEntryPermitCharges
buildStateToSEPCMap eligibleCandidates geomWithRingsMap =
  Map.fromList
    [ (gwr.geometry.state, sepcRow)
    | sepcRow <- eligibleCandidates,
      Just gwr <- [Map.lookup sepcRow.geomId geomWithRingsMap]
    ]

-- ---------------------------------------------------------------------------
-- In-memory point-in-polygon helpers
-- ---------------------------------------------------------------------------

-- | Pure: which candidate geometry IDs contain this point, checked against
-- pre-parsed polygon rings. No DB calls.
containingGeomIds ::
  LatLong ->
  Set.Set Text ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  Set.Set Text
containingGeomIds pt candidateGeomIds geomWithRingsMap =
  Set.fromList
    [ getId gId
    | (gId, gwr) <- Map.toList geomWithRingsMap,
      getId gId `Set.member` candidateGeomIds,
      Just polygons <- [gwr.rings],
      pointInMultiPolygon pt polygons
    ]

-- ---------------------------------------------------------------------------
-- Route segment traversal
-- ---------------------------------------------------------------------------

-- | Walk route segments and accumulate SEPC charges for each new state entered.
-- Each SEPC is charged at most once per route regardless of how many times
-- the route crosses into that state.
--
-- Pure after cache load — no DB calls inside the fold.
computeChargesAlongRouteSegments ::
  (MonadFlow m) =>
  [(LatLong, LatLong)] ->
  Set.Set Text ->
  Map.Map Context.IndianState StateEntryPermitCharges ->
  Map.Map (Id DGeo.Geometry) GeomWithRings ->
  Maybe (Id DP.Driver) ->
  m (HighPrecMoney, [Text], [Text])
computeChargesAlongRouteSegments routeSegments candidateGeomIds stateToSEPC geomWithRingsMap mbDriverId = do
  let driverTag = maybe "N/A" getId mbDriverId
  (total, names, ids, _) <-
    foldM
      ( \(accTotal, accNames, accIds, chargedSet) (p1, p2) -> do
          let geomIds1 = containingGeomIds p1 candidateGeomIds geomWithRingsMap
              geomIds2 = containingGeomIds p2 candidateGeomIds geomWithRingsMap
              statesOf gIds =
                nub
                  [ gwr.geometry.state
                  | (gId, gwr) <- Map.toList geomWithRingsMap,
                    getId gId `Set.member` gIds
                  ]
              states1 = statesOf geomIds1
              states2 = statesOf geomIds2
              newStates = states2 \\ states1
              sepcsToCharge =
                filter (\s -> getId s.id `Set.notMember` chargedSet) $
                  nubBy (\a b -> getId a.id == getId b.id) $
                    mapMaybe (`Map.lookup` stateToSEPC) newStates
              newCharged = chargedSet <> Set.fromList (map (getId . (.id)) sepcsToCharge)
              addTotal = sum $ map (.amount) sepcsToCharge
              addNames = map (fromMaybe "" . (.name)) sepcsToCharge
              addIds = map (getId . (.id)) sepcsToCharge

          unless (null sepcsToCharge) $
            logInfo $
              "SEPC: state transition detected | driverId: " <> driverTag
                <> " | p1 states: " <> show states1
                <> " | p2 states: " <> show states2
                <> " | new states entered: " <> show newStates
                <> " | charging: " <> show addIds

          pure (accTotal + addTotal, accNames <> addNames, accIds <> addIds, newCharged)
      )
      (0, [], [], Set.empty)
      routeSegments
  pure (total, names, ids)

-- ---------------------------------------------------------------------------
-- Main entry point
-- ---------------------------------------------------------------------------

-- | Returns SEPC charges (total, names, ids) for the given route, or Nothing if
-- no state entry permits apply.
--
-- Pipeline:
--   1. Load all SEPC entries + geometries from in-pod cache (DB hit only on miss).
--   2. Detect source state from first route point (pure, uses cached rings).
--   3. Filter candidates by allowed destination states.
--   4. Filter candidates by route bbox vs geometry bbox (cheap pre-filter).
--   5. Walk route segments with pure in-memory point-in-polygon (zero DB calls).
getStateEntryPermitInfoOnRoute ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasInMemEnv r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DP.Driver) ->
  RoutePoints ->
  m (Maybe (HighPrecMoney, [Text], [Text]))
getStateEntryPermitInfoOnRoute merchantOperatingCityId mbDriverId route = do
  let driverTag = maybe "N/A" getId mbDriverId
  logInfo $
    "SEPC: detection started | driverId: " <> driverTag
      <> " | merchantOpCityId: " <> getId merchantOperatingCityId
      <> " | route points: " <> show (length route)
  case route of
    [] -> logDebug "SEPC: empty route — skipping" >> pure Nothing
    [_] -> logDebug "SEPC: single-point route — skipping" >> pure Nothing
    firstPoint : _ -> do
      merchantOpCity <-
        CQMOC.findById merchantOperatingCityId
          >>= fromMaybeM (InternalError $ "MerchantOperatingCity not found: " <> getId merchantOperatingCityId)

      (allSEPCEntries, geomWithRingsMap) <- loadCachedSEPCEntriesWithGeometries
      logInfo $
        "SEPC cache read: fetched from cache"
          <> " | version: " <> sepcGeomCacheKeyVersion
          <> " | sepc entries: " <> show (length allSEPCEntries)
          <> " | geometries: " <> show (Map.size geomWithRingsMap)

      let sourceState = getSourceStateFromFirstPoint firstPoint merchantOpCity.state geomWithRingsMap
      logInfo $ "SEPC: source state | driverId: " <> driverTag <> " | state: " <> show sourceState

      allowedStates <- getAllowedDestinationStates merchantOpCity.merchantId sourceState mbDriverId

      let candidateSEPCs = filterSEPCsByAllowedState allSEPCEntries geomWithRingsMap allowedStates
      logInfo $
        "SEPC: after allowed-state filter | driverId: " <> driverTag
          <> " | candidates: " <> show (length candidateSEPCs)
          <> " | allowed states: " <> show allowedStates

      if null candidateSEPCs
        then do
          logInfo $ "SEPC: no candidates after state filter — skipping | driverId: " <> driverTag
          pure Nothing
        else do
          let routeBoundingBox = getBoundingBox route
          eligibleCandidates <- filterSEPCsByRouteBboxIntersection candidateSEPCs geomWithRingsMap routeBoundingBox
          logInfo $
            "SEPC: after bbox filter | driverId: " <> driverTag
              <> " | eligible candidates: " <> show (length eligibleCandidates)

          if null eligibleCandidates
            then do
              logInfo $ "SEPC: no candidates after bbox filter — skipping | driverId: " <> driverTag
              pure Nothing
            else do
              let candidateGeomIds = Set.fromList $ map (getId . (.geomId)) eligibleCandidates
                  stateToSEPC = buildStateToSEPCMap eligibleCandidates geomWithRingsMap
                  routeSegments = zip route (tail route)

              logDebug $
                "SEPC: starting segment traversal | driverId: " <> driverTag
                  <> " | segments: " <> show (length routeSegments)
                  <> " | candidate geom IDs: " <> show (Set.toList candidateGeomIds)

              (total, names, ids) <-
                computeChargesAlongRouteSegments
                  routeSegments
                  candidateGeomIds
                  stateToSEPC
                  geomWithRingsMap
                  mbDriverId

              if total > 0 && not (null names)
                then do
                  logInfo $
                    "SEPC: charges detected | driverId: " <> driverTag
                      <> " | total: " <> show total
                      <> " | names: " <> show names
                      <> " | ids: " <> show ids
                  pure $ Just (total, names, ids)
                else do
                  logInfo $ "SEPC: no charges after segment traversal | driverId: " <> driverTag
                  pure Nothing

-- ---------------------------------------------------------------------------
-- Allowed destination states
-- ---------------------------------------------------------------------------

-- | Allowed destination states for a given source state (from MerchantState
-- config). Falls back to [sourceState] when no config row exists.
getAllowedDestinationStates ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Context.IndianState ->
  Maybe (Id DP.Driver) ->
  m [Context.IndianState]
getAllowedDestinationStates merchantId sourceState mbDriverId = do
  let driverTag = maybe "N/A" getId mbDriverId
  mbMerchantState <- CQMS.findByMerchantIdAndState merchantId sourceState
  let allowedStates = maybe [sourceState] (.allowedDestinationStates) mbMerchantState
  logDebug $
    "SEPC: allowed destination states | driverId: " <> driverTag
      <> " | merchantId: " <> getId merchantId
      <> " | sourceState: " <> show sourceState
      <> " | allowed: " <> show allowedStates
  pure allowedStates

-- ---------------------------------------------------------------------------
-- End-ride reconciliation
-- ---------------------------------------------------------------------------

-- | At end of ride: reconcile estimated vs detected SEPC IDs and log the diff.
-- Returns (pendingIds, extraIds) where:
--   pendingIds = estimated but NOT detected (expected crossings that were missed)
--   extraIds   = detected but NOT in estimate (unexpected crossings)
-- No charge amounts are computed here; callers use ride fields directly.
-- Unlike tolls, SEPC has no "pending crossing" Redis key — crossing is detected
-- per segment and accumulated immediately.
checkAndValidatePendingStateEntryPermits ::
  (MonadFlow m) =>
  Id DRide.Ride ->
  Maybe [Text] ->
  Maybe [Text] ->
  m ([Text], [Text])
checkAndValidatePendingStateEntryPermits rideId mbEstimatedIds mbDetectedIds = do
  let estIds = fromMaybe [] mbEstimatedIds
      detIds = fromMaybe [] mbDetectedIds
      pendingIds = filter (`notElem` detIds) estIds
      extraIds = filter (`notElem` estIds) detIds
  case (mbEstimatedIds, mbDetectedIds) of
    (Nothing, Just _) ->
      logWarning $
        "SEPC: detected permits but no estimated IDs — NOT charging for safety"
          <> " | rideId: " <> rideId.getId
          <> " | detected: " <> show detIds
    _ ->
      logInfo $
        "SEPC: end-ride ID reconciliation"
          <> " | rideId: " <> rideId.getId
          <> " | estimated: " <> show estIds
          <> " | detected: " <> show detIds
          <> " | missed (in estimate, not detected): " <> show pendingIds
          <> " | extra (detected, not in estimate): " <> show extraIds
  pure (pendingIds, extraIds)
