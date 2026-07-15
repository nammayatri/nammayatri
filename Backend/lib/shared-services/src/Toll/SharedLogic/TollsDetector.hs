{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Toll.SharedLogic.TollsDetector
  ( checkAndValidatePendingTolls,
    clearTollStartGateBatchCache,
    filterRoutesPreferringToll,
    getTollInfoOnRoute,
    TollChargeDetails (..),
    TollInfo (..),
    emptyTollInfo,
    estimatedTollInfoFromCharges,
    hasDetectedTolls,
  )
where

import Data.List (nubBy)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.ComputeIntersection
import Lib.LocationUpdates.TollTypes
  ( TollChargeDetails (..),
    TollInfo (..),
    emptyTollInfo,
    estimatedTollInfoFromCharges,
    hasDetectedTolls,
  )
import Toll.Domain.Types.Toll
import Toll.Domain.Types.TollGate (TollGate (..), geoPolygonToLatLongRings, lineStringToSegments)
import Toll.Storage.BeamFlow (BeamFlow)
import Toll.Storage.CachedQueries.Toll (findAllTollsByMerchantOperatingCity)

tollStartGateTrackingKey :: Text -> Text
tollStartGateTrackingKey driverId = "TollGateTracking:DriverId-" <> driverId

-- This function is called during endRideTransaction to clear all pending tolls
clearTollStartGateBatchCache :: (CacheFlow m r) => Text -> m ()
clearTollStartGateBatchCache driverId = do
  Hedis.del $ tollStartGateTrackingKey driverId

gateIntersectsRouteSegment :: LineSegment -> TollGate -> Bool
gateIntersectsRouteSegment routeSegment = \case
  LineGate gateLine ->
    any (doIntersect routeSegment) (lineStringToSegments gateLine)
  PolyGate gatePolygon ->
    lineSegmentIntersectsPolygon routeSegment (geoPolygonToLatLongRings gatePolygon)

gateWithinBoundingBox :: BoundingBox -> TollGate -> Bool
gateWithinBoundingBox boundingBox = \case
  LineGate gateLine ->
    any (\seg -> lineSegmentWithinBoundingBox seg boundingBox) (lineStringToSegments gateLine)
  PolyGate gatePolygon ->
    polygonMayIntersectBoundingBox boundingBox (geoPolygonToLatLongRings gatePolygon)

pointInRing :: LatLong -> [LatLong] -> Bool
pointInRing _ ring | length ring < 3 = False
pointInRing (LatLong y x) ring =
  foldl' step False (zip ring (drop 1 ring ++ [head ring]))
  where
    step inside (LatLong y1 x1, LatLong y2 x2)
      | y1 == y2 = inside
      | ((y1 > y) /= (y2 > y))
          && (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1) =
        not inside
      | otherwise = inside

pointInPolygon :: LatLong -> [[LatLong]] -> Bool
pointInPolygon _ [] = False
pointInPolygon p (outer : holes) = pointInRing p outer && not (any (pointInRing p) holes)

ringEdges :: [LatLong] -> [LineSegment]
ringEdges ring
  | length ring < 2 = []
  | otherwise = zipWith LineSegment ring (drop 1 ring ++ [head ring])

lineSegmentIntersectsPolygon :: LineSegment -> [[LatLong]] -> Bool
lineSegmentIntersectsPolygon routeSegment rings =
  let LineSegment startPoint endPoint = routeSegment
      intersectsBoundary = any (\ring -> any (doIntersect routeSegment) (ringEdges ring)) rings
      entersOrInside = pointInPolygon startPoint rings || pointInPolygon endPoint rings
   in intersectsBoundary || entersOrInside

boundingBoxEdges :: BoundingBox -> [LineSegment]
boundingBoxEdges boundingBox =
  [ LineSegment boundingBox.topLeft boundingBox.topRight,
    LineSegment boundingBox.topRight boundingBox.bottomRight,
    LineSegment boundingBox.bottomRight boundingBox.bottomLeft,
    LineSegment boundingBox.bottomLeft boundingBox.topLeft
  ]

polygonMayIntersectBoundingBox :: BoundingBox -> [[LatLong]] -> Bool
polygonMayIntersectBoundingBox bbox rings =
  let ringPoints = concat rings
      anyPolyPointInside = any (`pointWithinBoundingBox` bbox) ringPoints
      anyBoxCornerInsidePoly =
        any
          (`pointInPolygon` rings)
          [bbox.topLeft, bbox.topRight, bbox.bottomLeft, bbox.bottomRight]
      anyEdgeIntersects =
        any
          (\polyEdge -> any (doIntersect polyEdge) (boundingBoxEdges bbox))
          (concatMap ringEdges rings)
   in anyPolyPointInside || anyBoxCornerInsidePoly || anyEdgeIntersects

-- | Validates pending tolls (entry detected, exit not found) against estimated tolls using IDs
-- | Used at end ride to apply toll charges when exit gate was never detected
-- | Returns Nothing if validation fails or no estimate exists (conservative approach for safety)
-- | Now uses toll IDs for exact matching and handles partial toll scenarios (some detected, some not)
checkAndValidatePendingTolls ::
  (CacheFlow m r) =>
  Text ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] -> -- Estimated toll IDs
  Maybe HighPrecMoney ->
  Maybe [Text] -> -- Already detected toll IDs
  m (Maybe (HighPrecMoney, [Text], [Text]))
checkAndValidatePendingTolls driverId _estimatedTollCharges _estimatedTollNames estimatedTollIds alreadyDetectedCharges alreadyDetectedTollIds = do
  mbPendingTolls :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)

  case (mbPendingTolls, estimatedTollIds, alreadyDetectedTollIds) of
    (Just pendingTolls, Just estIds, Just detectedIds) -> do
      -- Find which estimated tolls are MISSING from already detected ones
      let missingTollIds = filter (`notElem` detectedIds) estIds

      if null missingTollIds
        then do
          -- All estimated tolls already detected
          logInfo "All estimated tolls already detected, no pending tolls to apply"
          return Nothing
        else do
          -- Find pending tolls that match missing IDs
          let matchingPendingTolls = filter (\toll -> getId toll.id `elem` missingTollIds) pendingTolls

          if not $ null matchingPendingTolls
            then do
              -- Calculate charges only for MISSING tolls
              let missingCharges = sum $ map (.price.amount) matchingPendingTolls
                  missingNames = map (.name) matchingPendingTolls
                  missingIds = map (getId . (.id)) matchingPendingTolls

              logInfo $
                "Applying charges for missing tolls: "
                  <> show (zip missingNames (map (.price.amount) matchingPendingTolls))
                  <> " | Already detected: "
                  <> show (fromMaybe 0 alreadyDetectedCharges)

              return $ Just (missingCharges, missingNames, missingIds)
            else do
              logWarning $
                "Missing toll IDs don't match any cached pending tolls. "
                  <> "Missing: "
                  <> show missingTollIds
                  <> " | Cached: "
                  <> show (map (getId . (.id)) pendingTolls)
              return Nothing
    (Just pendingTolls, Just estIds, Nothing) -> do
      -- No tolls detected yet, validate all estimated tolls against pending
      let matchingPendingTolls = filter (\toll -> getId toll.id `elem` estIds) pendingTolls

      if not $ null matchingPendingTolls
        then do
          let charges = sum $ map (.price.amount) matchingPendingTolls
              names = map (.name) matchingPendingTolls
              ids = map (getId . (.id)) matchingPendingTolls

          logInfo $
            "No tolls detected yet, applying estimated tolls: "
              <> show (zip names (map (.price.amount) matchingPendingTolls))

          return $ Just (charges, names, ids)
        else do
          logWarning "Estimated toll IDs don't match any cached pending tolls"
          return Nothing
    (Just pendingTolls, Nothing, _) -> do
      -- Entry detected but NO estimated toll IDs - don't charge without validation
      logWarning $
        "Pending toll found WITHOUT estimate IDs. NOT charging for safety: "
          <> show (map (\t -> (t.name, t.price.amount)) pendingTolls)
      return Nothing
    (Nothing, _, _) ->
      -- No pending tolls in cache
      return Nothing

type TollChargesAndNamesAndIds = TollInfo

addTollChargeDetails :: Toll -> TollChargeDetails -> TollChargeDetails
addTollChargeDetails toll@Toll {..} TollChargeDetails {..} =
  TollChargeDetails
    { tollCharges = tollCharges + price.amount,
      tollNames = tollNames <> [name],
      tollIds = tollIds <> [getId toll.id]
    }

addTollChargeDetailsIf :: Bool -> Toll -> TollChargeDetails -> TollChargeDetails
addTollChargeDetailsIf shouldAdd toll tollChargeDetails =
  if shouldAdd then addTollChargeDetails toll tollChargeDetails else tollChargeDetails

addDetectedTollCharge :: Toll -> TollChargesAndNamesAndIds -> TollChargesAndNamesAndIds
addDetectedTollCharge toll@Toll {..} acc =
  let updatedOverall = addTollChargeDetails toll (TollChargeDetails acc.tollCharges acc.tollNames acc.tollIds)
   in acc{tollCharges = updatedOverall.tollCharges,
          tollNames = updatedOverall.tollNames,
          tollIds = updatedOverall.tollIds,
          isAutoRickshawAllowed = acc.isAutoRickshawAllowed && isAutoRickshawAllowed,
          isTwoWheelerAllowed = Just (fromMaybe True acc.isTwoWheelerAllowed && fromMaybe True isTwoWheelerAllowed),
          autoRickshawTollChargeDetails =
            addTollChargeDetailsIf (fromMaybe False isAutoRickshawTollChargeApplicable) toll acc.autoRickshawTollChargeDetails,
          twoWheelerTollChargeDetails =
            addTollChargeDetailsIf (fromMaybe False isTwoWheelerTollChargeApplicable) toll acc.twoWheelerTollChargeDetails
         }

addTollCharge :: Toll -> TollChargesAndNamesAndIds -> TollChargesAndNamesAndIds
addTollCharge = addDetectedTollCharge

tollStartsOnRouteSegment :: LineSegment -> Toll -> Bool
tollStartsOnRouteSegment routeSegment Toll {..} =
  any (gateIntersectsRouteSegment routeSegment) tollStartGates

usesPolygonStartGates :: Toll -> Bool
usesPolygonStartGates Toll {..} =
  any isPolyStartGate tollStartGates
  where
    isPolyStartGate (PolyGate _) = True
    isPolyStartGate (LineGate _) = False

-- | True when any pending toll's exit gate is crossed on this route segment.
exitDetectedOnRouteSegment :: LineSegment -> [Toll] -> Bool
exitDetectedOnRouteSegment routeSegment =
  any (\Toll {..} -> any (gateIntersectsRouteSegment routeSegment) tollEndGates)

-- | On segments where a pending toll exits, only line tolls may be newly armed.
canAddTollToPending :: Bool -> Toll -> Bool
canAddTollToPending exitOnSegment toll =
  not exitOnSegment || not (usesPolygonStartGates toll)

addPendingTollStartersOnSegment :: LineSegment -> [Toll] -> [Toll] -> [Toll]
addPendingTollStartersOnSegment routeSegment tolls pendingTolls =
  let pendingTollIds = map (getId . (.id)) pendingTolls
      exitOnSegment = exitDetectedOnRouteSegment routeSegment pendingTolls
      newTollStarters =
        filter
          ( \toll ->
              tollStartsOnRouteSegment routeSegment toll
                && getId toll.id `notElem` pendingTollIds
                && canAddTollToPending exitOnSegment toll
          )
          tolls
   in nubBy (\toll1 toll2 -> getId toll1.id == getId toll2.id) (pendingTolls <> newTollStarters)

-- | When a toll exit is found, charge it and drop every pending toll that shares the same entry gate.
resolveTollExitsOnSegment :: LineSegment -> [Toll] -> TollChargesAndNamesAndIds -> ([Toll], TollChargesAndNamesAndIds)
resolveTollExitsOnSegment routeSegment pendingTolls tollChargesAndNamesAndIds =
  case find (\Toll {..} -> any (gateIntersectsRouteSegment routeSegment) tollEndGates) pendingTolls of
    Nothing -> (pendingTolls, tollChargesAndNamesAndIds)
    Just matchedToll ->
      let remainingPendingTolls = filter (\toll -> toll.tollStartGates /= matchedToll.tollStartGates) pendingTolls
       in resolveTollExitsOnSegment routeSegment remainingPendingTolls (addTollCharge matchedToll tollChargesAndNamesAndIds)

cachePendingTolls :: (CacheFlow m r) => Maybe Text -> [Toll] -> m ()
cachePendingTolls _ [] = pure ()
cachePendingTolls Nothing _ = pure ()
cachePendingTolls (Just driverId) pendingTolls =
  Hedis.setExp (tollStartGateTrackingKey driverId) pendingTolls 21600 -- 6 hours

-- | Walks the route segment-by-segment using the same start/exit pairing as the original
-- | algorithm: tolls whose entry gate is crossed are tracked in pendingTolls until their
-- | exit gate is crossed, at which point they are charged. Unlike the original slice-based
-- | walk, the route is not truncated after each exit so nested tolls inside an active toll
-- | window are still detected on later segments.
getAggregatedTollChargesAndNamesOnRoute ::
  (CacheFlow m r) =>
  Maybe Text ->
  RoutePoints ->
  [Toll] ->
  [Toll] ->
  TollChargesAndNamesAndIds ->
  m TollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute mbDriverId route tolls initialPendingTolls tollChargesAndNamesAndIds = do
  let (finalTollChargesAndNamesAndIds, finalPendingTolls) =
        go initialPendingTolls route tolls tollChargesAndNamesAndIds
  if null finalPendingTolls
    then whenJust mbDriverId $ \driverId -> Hedis.del $ tollStartGateTrackingKey driverId
    else cachePendingTolls mbDriverId finalPendingTolls
  return finalTollChargesAndNamesAndIds
  where
    go pendingTolls [] _ acc = (acc, pendingTolls)
    go pendingTolls [_] _ acc = (acc, pendingTolls)
    go pendingTolls _ [] acc = (acc, pendingTolls)
    go pendingTolls (p1 : p2 : ps) tolls' acc =
      let currentRouteSegment = LineSegment p1 p2
          pendingWithStarters = addPendingTollStartersOnSegment currentRouteSegment tolls' pendingTolls
          (remainingPendingTolls, updatedAcc) =
            resolveTollExitsOnSegment currentRouteSegment pendingWithStarters acc
       in go remainingPendingTolls (p2 : ps) tolls' updatedAcc

{- Author: Khuzema Khomosi
  Best Case Time Complexity - O(No. of points in routes)
  Worst Case Time Complexity - O(No. of points in route * No. of gate segments of eligible tolls)

  This function first finds all the eligible Tolls whose entry segments lie within the Route's bounding box.
  For each route segment it finds tolls whose entry gate intersects, tracks them as pending until the
  corresponding exit gate is crossed, then charges them. The route is not sliced after each exit so
  nested tolls inside an active toll window are still detected on later segments.

  On segments where a pending toll exits, new polygon tolls are not armed (line tolls still may be).

  Note:
  In case of on ride it is possible that the batch of driver waypoints that we have has only the start of the toll and the end of the toll comes later.
  In that case this function maintains pending tolls in Redis based on driverId to resolve exits in later batches.
-}
getTollInfoOnRoute :: (BeamFlow m r, EsqDBReplicaFlow m r) => Text -> Maybe Text -> RoutePoints -> m (Maybe TollInfo)
getTollInfoOnRoute merchantOperatingCityId mbDriverId route = do
  tolls <- B.runInReplica $ findAllTollsByMerchantOperatingCity merchantOperatingCityId
  if not $ null tolls
    then do
      let boundingBox = getBoundingBox route
          eligibleTollsThatMaybePresentOnTheRoute = filter (\toll -> any (gateWithinBoundingBox boundingBox) toll.tollStartGates) tolls
      case mbDriverId of
        Just driverId -> do
          mbTollCombinationsWithStartGatesInPrevBatch :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey driverId)
          case mbTollCombinationsWithStartGatesInPrevBatch of
            Just tollCombinationsWithStartGatesInPrevBatch -> getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute
            Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute emptyTollInfo
        Nothing -> getAggregatedTollCharges route eligibleTollsThatMaybePresentOnTheRoute emptyTollInfo
    else return Nothing
  where
    getAggregatedTollCharges remainingRoute eligibleTollsThatMaybePresentOnTheRoute initialTollInfo = do
      aggregatedTollInfo <-
        getAggregatedTollChargesAndNamesOnRoute mbDriverId remainingRoute eligibleTollsThatMaybePresentOnTheRoute [] initialTollInfo
      if hasDetectedTolls aggregatedTollInfo
        then return $ Just aggregatedTollInfo
        else return Nothing

    getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide driverId tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute = do
      aggregatedTollInfo <-
        getAggregatedTollChargesAndNamesOnRoute
          (Just driverId)
          route
          eligibleTollsThatMaybePresentOnTheRoute
          tollCombinationsWithStartGatesInPrevBatch
          emptyTollInfo
      if hasDetectedTolls aggregatedTollInfo
        then return $ Just aggregatedTollInfo
        else return Nothing

-- | Filter Google route alternatives down to those that traverse at least one toll
--   (start gate intersected AND its corresponding exit gate intersected on the same polyline).
--   When the filter empties (no alternative uses tolls), the original list is returned with
--   a warning logged — graceful fallback so an enforce-toll search never fails outright.
--   Used by both rider-app and BPP search handlers when pickup or drop is in a SpecialLocation
--   with enforceTollRoute=True.
filterRoutesPreferringToll ::
  (BeamFlow m r, EsqDBReplicaFlow m r) =>
  Text ->
  [Maps.RouteInfo] ->
  m [Maps.RouteInfo]
filterRoutesPreferringToll merchantOpCityId routes = do
  withTollFlag <- forM routes $ \r -> do
    mbToll <- getTollInfoOnRoute merchantOpCityId Nothing r.points
    pure (r, isJust mbToll)
  let tollUsing = map fst $ filter snd withTollFlag
  if null tollUsing
    then do
      logWarning $
        "filterRoutesPreferringToll: enforce-toll requested but none of "
          <> show (length routes)
          <> " alternatives use tolls in merchantOpCity="
          <> merchantOpCityId
          <> "; falling back to full set."
      pure routes
    else pure tollUsing
