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
import Domain.Types.StateEntryPermitCharges
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.InMem as IM
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.External.Maps.Types (LatLong)
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantState as CQMS
import Storage.Queries.Geometry (findGeometriesContainingGps, findGeometryById)
import Storage.Queries.StateEntryPermitCharges (findByPrimaryKey)
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
boundingBoxesIntersect :: BoundingBox -> Maybe BoundingBoxPoints -> Bool
boundingBoxesIntersect _ Nothing = False
boundingBoxesIntersect rbb (Just sbbPoints) =
  case boundingBoxPointsToLineSegments sbbPoints of
    Nothing -> True
    Just sbbSides ->
      let rbbSides = boundingBoxToLineSegments rbb
       in any (\rbbSide -> any (doIntersect rbbSide) sbbSides) rbbSides

-- | Cache key version for in-pod SEPC+geometry cache. TTL is 12 hours; bump this when geom data changes.
sepcGeomCacheKeyVersion :: Text
sepcGeomCacheKeyVersion = "geom:v1"

-- | Load all SEPC entries and their geometry rows, using in-pod cache so we hit DB at most once per TTL per pod.
loadCachedSEPCEntriesWithGeometries ::
  (MonadFlow m, EsqDBFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, HasInMemEnv r) =>
  m ([StateEntryPermitCharges], Map.Map (Id DGeo.Geometry) DGeo.Geometry)
loadCachedSEPCEntriesWithGeometries =
  IM.withInMemCache ["CACHED_SEPC_GEOMS", sepcGeomCacheKeyVersion] 43200 $ do
    allSEPCEntries <- findAllStateEntryPermitCharges
    let uniqueGeomIds = nub $ map (.geomId) allSEPCEntries
    geomsMaybe <- mapM findGeometryById uniqueGeomIds
    let geometryByGeomId = Map.fromList [(g.id, g) | Just g <- geomsMaybe]
    pure (allSEPCEntries, geometryByGeomId :: Map.Map (Id DGeo.Geometry) DGeo.Geometry)

-- | Derive the source state from the first route point (which geometries contain it), or use fallback if none.
getSourceStateFromFirstPoint ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  LatLong ->
  Context.IndianState ->
  m Context.IndianState
getSourceStateFromFirstPoint firstPoint fallbackState = do
  geoms <- findGeometriesContainingGps firstPoint
  pure $ fromMaybe fallbackState (DGeo.state <$> listToMaybe geoms)

-- | Allowed destination states for a given source state (from MerchantState config, or just source if not configured).
getAllowedDestinationStates ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Context.IndianState ->
  m [Context.IndianState]
getAllowedDestinationStates merchantId sourceState = do
  mbMerchantState <- CQMS.findByMerchantIdAndState merchantId sourceState
  pure $ maybe [sourceState] (.allowedDestinationStates) mbMerchantState

-- | Keep only SEPC entries whose geometry's state is in the allowed destination states list.
filterSEPCsByAllowedState ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) DGeo.Geometry ->
  [Context.IndianState] ->
  [StateEntryPermitCharges]
filterSEPCsByAllowedState allSEPCEntries geometryByGeomId allowedStates =
  filter hasAllowedState allSEPCEntries
  where
    hasAllowedState sepcRow =
      case Map.lookup sepcRow.geomId geometryByGeomId of
        Nothing -> False
        Just geomRow -> geomRow.state `elem` allowedStates

-- | Keep only SEPC entries whose geometry bounding box intersects the route bounding box (drops null bbox / no intersection).
filterSEPCsByRouteBboxIntersection ::
  [StateEntryPermitCharges] ->
  Map.Map (Id DGeo.Geometry) DGeo.Geometry ->
  BoundingBox ->
  [StateEntryPermitCharges]
filterSEPCsByRouteBboxIntersection candidateSEPCs geometryByGeomId routeBoundingBox =
  filter intersectsRouteBbox candidateSEPCs
  where
    intersectsRouteBbox sepcRow =
      case Map.lookup sepcRow.geomId geometryByGeomId of
        Nothing -> False
        Just geomRow -> boundingBoxesIntersect routeBoundingBox geomRow.bbox

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
getStateEntryPermitInfoOnRoute merchantOperatingCityId _mbDriverId route =
  case route of
    [] -> pure Nothing
    [_] -> pure Nothing
    firstPoint : _ -> do
      merchantOpCity <- CQMOC.findById merchantOperatingCityId >>= fromMaybeM (InternalError "MerchantOperatingCity not found")

      sourceState <- getSourceStateFromFirstPoint firstPoint merchantOpCity.state
      allowedStates <- getAllowedDestinationStates merchantOpCity.merchantId sourceState

      (allSEPCEntries, geometryByGeomId) <- loadCachedSEPCEntriesWithGeometries

      let candidateSEPCsWithState = filterSEPCsByAllowedState allSEPCEntries geometryByGeomId allowedStates

      if null candidateSEPCsWithState
        then pure Nothing
        else do
          let routeBoundingBox = getBoundingBox route
          let eligibleCandidates =
                filterSEPCsByRouteBboxIntersection candidateSEPCsWithState geometryByGeomId routeBoundingBox

          if null eligibleCandidates
            then pure Nothing
            else do
              let candidateGeomIds = Set.fromList $ map (getId . (.geomId)) eligibleCandidates
              let stateToSEPC = buildStateToSEPCMap eligibleCandidates geometryByGeomId
              let routeSegments = zip route (tail route)

              (total, names, ids) <-
                computeChargesAlongRouteSegments routeSegments candidateGeomIds stateToSEPC

              if total > 0 && not (null names)
                then pure $ Just (total, names, ids)
                else pure Nothing

-- | At end ride: compare estimated SEPC IDs vs detected IDs. For any estimated ID not in detected,
-- look up SEPC by ID and include in the result (so we can apply estimated-for-missing logic upstream).
-- No Redis pending key for SEPC; this is pure comparison + DB lookup.
checkAndValidatePendingStateEntryPermits ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, HasField "hedisMigrationStage" r Bool) =>
  Maybe [Text] ->
  Maybe [Text] ->
  m (Maybe (HighPrecMoney, [Text], [Text]))
checkAndValidatePendingStateEntryPermits estimatedIds alreadyDetectedIds = do
  case (estimatedIds, alreadyDetectedIds) of
    (Just estIds, Just detectedIds) -> do
      let missingIds = filter (`notElem` detectedIds) estIds
      if null missingIds
        then pure Nothing
        else lookupAndSum missingIds
    (Just estIds, Nothing) -> lookupAndSum estIds
    _ -> pure Nothing
  where
    lookupAndSum ids = do
      mbSEPCs <- mapM (findByPrimaryKey . Id) ids
      let listOfSEPCs = catMaybes mbSEPCs
      if null listOfSEPCs
        then pure Nothing
        else
          pure $
            Just
              ( sum $ map (.amount) listOfSEPCs,
                map (fromMaybe "" . (.name)) listOfSEPCs,
                map (getId . (.id)) listOfSEPCs
              )
