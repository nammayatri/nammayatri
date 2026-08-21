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
    TollTracking (..),
    TollTrackingScope (..),
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
    onRideTollIdsRedisKey,
  )
import Toll.Domain.Types.Toll
import Toll.Domain.Types.TollGate (TollGate (..), geoPolygonToLatLongRings, lineStringToSegments)
import Toll.Storage.BeamFlow (BeamFlow)
import Toll.Storage.CachedQueries.Toll (findAllTollsByMerchantOperatingCity)

-- | Which consumer owns a pending-toll state machine. Each scope gets its own Redis key so that
--   the deviation walk (live raw waypoints, charges discarded) can never consume or corrupt the
--   pending tolls of the billing walk (lagging snapped batches, charges applied).
data TollTrackingScope
  = TollTrackingSnapToRoad
  | TollTrackingDeviation
  deriving (Show, Eq, Enum, Bounded)

data TollTracking = TollTracking
  { trackingScope :: TollTrackingScope,
    trackingDriverId :: Text
  }
  deriving (Show, Eq)

tollTrackingKeyScopeTag :: TollTrackingScope -> Text
tollTrackingKeyScopeTag TollTrackingSnapToRoad = ""
tollTrackingKeyScopeTag TollTrackingDeviation = "Deviation:"

tollStartGateTrackingKey :: TollTrackingScope -> Text -> Text
tollStartGateTrackingKey scope driverId =
  "TollGateTracking:" <> tollTrackingKeyScopeTag scope <> "DriverId-" <> driverId

allTollTrackingScopes :: [TollTrackingScope]
allTollTrackingScopes = [minBound .. maxBound]

-- This function is called during endRideTransaction to clear all pending tolls
clearTollStartGateBatchCache :: (CacheFlow m r) => Text -> m ()
clearTollStartGateBatchCache driverId =
  forM_ allTollTrackingScopes $ \scope ->
    Hedis.del $ tollStartGateTrackingKey scope driverId

-- | A LineGate start is crossed when the current route segment intersects it.
--   A PolyGate start is crossed only when the route was inside the polygon on the previous segment
--   and the current segment now meets its boundary, i.e. the route is leaving the polygon. That
--   direction requirement is what distinguishes an inbound from an outbound toll.
gateStartsOnRouteSegment :: Maybe LineSegment -> LineSegment -> TollGate -> Bool
gateStartsOnRouteSegment mbPreviousSegment routeSegment = \case
  LineGate gateLine ->
    any (doIntersect routeSegment) (lineStringToSegments gateLine)
  PolyGate gatePolygon ->
    let rings = geoPolygonToLatLongRings gatePolygon
     in maybe False (`lineSegmentWithinPolygon` rings) mbPreviousSegment
          && lineSegmentIntersectsPolygonBoundary routeSegment rings
  where
    lineSegmentWithinPolygon :: LineSegment -> [[LatLong]] -> Bool
    lineSegmentWithinPolygon (LineSegment startPoint endPoint) rings =
      pointInPolygon startPoint rings && pointInPolygon endPoint rings

-- | An end gate is crossed on the same condition as a LineGate, except that a PolyGate also counts
--   as crossed when the route segment merely overlaps the polygon.
gateEndsOnRouteSegment :: LineSegment -> TollGate -> Bool
gateEndsOnRouteSegment routeSegment = \case
  LineGate gateLine ->
    any (doIntersect routeSegment) (lineStringToSegments gateLine)
  PolyGate gatePolygon ->
    lineSegmentIntersectsPolygonBoundary routeSegment (geoPolygonToLatLongRings gatePolygon)

gateWithinBoundingBox :: BoundingBox -> TollGate -> Bool
gateWithinBoundingBox boundingBox = \case
  LineGate gateLine ->
    any (\seg -> lineSegmentWithinBoundingBox seg boundingBox) (lineStringToSegments gateLine)
  PolyGate gatePolygon ->
    polygonMayIntersectBoundingBox boundingBox (geoPolygonToLatLongRings gatePolygon)
  where
    polygonMayIntersectBoundingBox :: BoundingBox -> [[LatLong]] -> Bool
    polygonMayIntersectBoundingBox bbox rings =
      let anyPolyPointInside = any (`pointWithinBoundingBox` bbox) (concat rings)
          anyBoxCornerInsidePoly =
            any
              (`pointInPolygon` rings)
              [bbox.topLeft, bbox.topRight, bbox.bottomLeft, bbox.bottomRight]
          anyEdgeIntersects =
            any
              (\polyEdge -> any (doIntersect polyEdge) (boundingBoxEdges bbox))
              (concatMap ringEdges rings)
       in anyPolyPointInside || anyBoxCornerInsidePoly || anyEdgeIntersects

pointInPolygon :: LatLong -> [[LatLong]] -> Bool
pointInPolygon _ [] = False
pointInPolygon p (outer : holes) = pointInRing p outer && not (any (pointInRing p) holes)
  where
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

ringEdges :: [LatLong] -> [LineSegment]
ringEdges ring
  | length ring < 2 = []
  | otherwise = zipWith LineSegment ring (drop 1 ring ++ [head ring])

lineSegmentIntersectsPolygonBoundary :: LineSegment -> [[LatLong]] -> Bool
lineSegmentIntersectsPolygonBoundary routeSegment rings =
  any (\ring -> any (doIntersect routeSegment) (ringEdges ring)) rings

boundingBoxEdges :: BoundingBox -> [LineSegment]
boundingBoxEdges boundingBox =
  [ LineSegment boundingBox.topLeft boundingBox.topRight,
    LineSegment boundingBox.topRight boundingBox.bottomRight,
    LineSegment boundingBox.bottomRight boundingBox.bottomLeft,
    LineSegment boundingBox.bottomLeft boundingBox.topLeft
  ]

-- | Validates pending tolls (entry detected, exit not found) against estimated tolls using IDs
-- | Used at end ride to apply toll charges when exit gate was never detected
-- | Returns Nothing if validation fails or no estimate exists (conservative approach for safety)
-- | Now uses toll IDs for exact matching and handles partial toll scenarios (some detected, some not)
checkAndValidatePendingTolls ::
  (CacheFlow m r) =>
  TollTrackingScope ->
  Text ->
  Maybe HighPrecMoney ->
  Maybe [Text] ->
  Maybe [Text] -> -- Estimated toll IDs
  Maybe HighPrecMoney ->
  Maybe [Text] -> -- Already detected toll IDs
  m (Maybe (HighPrecMoney, [Text], [Text]))
checkAndValidatePendingTolls scope driverId _estimatedTollCharges _estimatedTollNames estimatedTollIds alreadyDetectedCharges alreadyDetectedTollIds = do
  mbPendingTolls :: Maybe [Toll] <- Hedis.safeGet (tollStartGateTrackingKey scope driverId)
  chargedTollIds <- getChargedTollIdsInRide driverId

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
          let matchingPendingTolls = filter (\toll -> getId toll.id `elem` missingTollIds && withinMaxTollCountInRide chargedTollIds toll) pendingTolls

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
      let matchingPendingTolls = filter (\toll -> getId toll.id `elem` estIds && withinMaxTollCountInRide chargedTollIds toll) pendingTolls

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

withinMaxTollCountInRide :: [Text] -> Toll -> Bool
withinMaxTollCountInRide chargedTollIds toll =
  maybe True (length (filter (== getId toll.id) chargedTollIds) <) toll.maxTollCountInRide

addTollChargeWithinMaxCount :: [Text] -> Toll -> TollChargesAndNamesAndIds -> TollChargesAndNamesAndIds
addTollChargeWithinMaxCount chargedTollIds toll acc =
  if withinMaxTollCountInRide (chargedTollIds <> acc.tollIds) toll then addTollCharge toll acc else acc

getChargedTollIdsInRide :: (CacheFlow m r) => Text -> m [Text]
getChargedTollIdsInRide driverId = Hedis.lRange (onRideTollIdsRedisKey driverId) 0 (-1)

tollStartsOnRouteSegment :: Maybe LineSegment -> LineSegment -> Toll -> Bool
tollStartsOnRouteSegment mbPreviousSegment routeSegment Toll {..} =
  any (gateStartsOnRouteSegment mbPreviousSegment routeSegment) tollStartGates

tollEndsOnRouteSegment :: LineSegment -> Toll -> Bool
tollEndsOnRouteSegment routeSegment Toll {..} =
  any (gateEndsOnRouteSegment routeSegment) tollEndGates

-- Removes the matched toll AND all other possibilities with the same entry gate from pending cache
-- For example, if XL is matched (X entry, L exit), removes XK, XL, XM (all with entry X)
removeMatchedTollFromCache :: (CacheFlow m r) => TollTracking -> [Toll] -> Toll -> m ()
removeMatchedTollFromCache tracking allPendingTolls matchedToll = do
  let key = tollStartGateTrackingKey tracking.trackingScope tracking.trackingDriverId
      remainingPendingTolls = filter (\toll -> toll.tollStartGates /= matchedToll.tollStartGates) allPendingTolls
  if null remainingPendingTolls
    then Hedis.del key
    else Hedis.setExp key remainingPendingTolls 21600 -- 6 hours

-- This function is triggered when allTollCombinationsWithStartGates are found intersecting the route to find the exit segment intersection on the further route and return the segment it was found on along with the remaining route after intersection.
getExitTollAndRemainingRoute :: RoutePoints -> [Toll] -> Maybe (LineSegment, RoutePoints, Toll)
getExitTollAndRemainingRoute [] _ = Nothing
getExitTollAndRemainingRoute [_] _ = Nothing
getExitTollAndRemainingRoute _ [] = Nothing
getExitTollAndRemainingRoute (p1 : p2 : ps) tolls =
  let exitSegment = LineSegment p1 p2
   in case find (tollEndsOnRouteSegment exitSegment) tolls of
        Just toll -> Just (exitSegment, p2 : ps, toll)
        Nothing -> getExitTollAndRemainingRoute (p2 : ps) tolls

-- This function is responsible for checking the toll start and exit segments intersection on the route and appropriately update the state in case exit segment is not found corresponding to the entry segment while On Ride.
getAggregatedTollChargesAndNamesOnRoute ::
  (CacheFlow m r) =>
  Maybe TollTracking ->
  [Text] ->
  Maybe LineSegment ->
  RoutePoints ->
  [Toll] ->
  TollChargesAndNamesAndIds ->
  m TollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute _ _ _ [] _ tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute _ _ _ [_] _ tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute _ _ _ _ [] tollChargesAndNamesAndIds = return tollChargesAndNamesAndIds
getAggregatedTollChargesAndNamesOnRoute mbTracking chargedTollIds mbPreviousSegment route@(p1 : p2 : ps) tolls tollChargesAndNamesAndIds = do
  let currentRouteSegment = LineSegment p1 p2
      allTollCombinationsWithStartGates = filter (tollStartsOnRouteSegment mbPreviousSegment currentRouteSegment) tolls
  if not $ null allTollCombinationsWithStartGates
    then do
      case getExitTollAndRemainingRoute route allTollCombinationsWithStartGates of
        Just (exitSegment, remainingRoute, toll) ->
          getAggregatedTollChargesAndNamesOnRoute
            mbTracking
            chargedTollIds
            (Just exitSegment)
            remainingRoute
            tolls
            (addTollChargeWithinMaxCount chargedTollIds toll tollChargesAndNamesAndIds)
        Nothing -> do
          whenJust mbTracking $ \tracking -> do
            let key = tollStartGateTrackingKey tracking.trackingScope tracking.trackingDriverId
            mbExistingPendingTolls :: Maybe [Toll] <- Hedis.safeGet key
            let allPendingTolls = fromMaybe [] mbExistingPendingTolls <> allTollCombinationsWithStartGates
                uniquePendingTolls = nubBy (\toll1 toll2 -> getId toll1.id == getId toll2.id) allPendingTolls
            Hedis.setExp key uniquePendingTolls 21600 -- 6 hours
          return tollChargesAndNamesAndIds
    else getAggregatedTollChargesAndNamesOnRoute mbTracking chargedTollIds (Just currentRouteSegment) (p2 : ps) tolls tollChargesAndNamesAndIds

{- Author: Khuzema Khomosi
  Best Case Time Complexity - O(No. of points in routes)
  Worst Case Time Complexity - O(No. of points in route * No. of gate segments of eligible tolls)

  This function first finds all the eligible Tolls whose entry gates lie within the Route's bounding box.
  For each two points of the route, it finds all the tolls whose entry gates intersect the two points of the route.
  If it finds some entry gates of tolls intersecting the route points, then it goes further on the route to find the exit gate intersecting the toll.
  Once the exit gate is found, it add's the toll and slices the route further to check for anymore tolls if exists till it reaches the end of the route.

  A gate may be a LineGate or a PolyGate; a PolyGate is treated as the closed run of line segments
  forming its boundary, so both kinds are crossed on exactly the same condition.

  Note:
  In case of on ride it is possible that the batch of driver waypoints that we have has only the start of the toll and the end of the toll comes later.
  In that case this function maintains the TollCombinationsWithStartGatesInPrevBatch, keyed by TollTrackingScope and driverId, to check for it's corresponding exit gate intersection on route coming in later batches.
  Each scope owns its own key: a caller that discards the computed charges (e.g. the toll-route
  deviation check) must never share pending state with the caller that bills them.
  Pass Nothing for the tracking to run statelessly against a route without touching Redis.

  A PolyGate start needs the segment preceding the current one, which for the first segment of a
  batch lies in the previous batch. The caller owns batch continuity, so it supplies that segment;
  Nothing means the route is being walked from its true beginning.
-}
getTollInfoOnRoute :: (BeamFlow m r, EsqDBReplicaFlow m r) => Text -> Maybe TollTracking -> Maybe LineSegment -> RoutePoints -> m (Maybe TollInfo)
getTollInfoOnRoute merchantOperatingCityId mbTracking previousSegment route = do
  tolls <- filter (\toll -> toll.price.amount > 0) <$> B.runInReplica (findAllTollsByMerchantOperatingCity merchantOperatingCityId)
  chargedTollIdsInRide <- maybe (pure []) (getChargedTollIdsInRide . (.trackingDriverId)) mbTracking
  if not $ null tolls
    then do
      let boundingBox = getBoundingBox route
          eligibleTollsThatMaybePresentOnTheRoute = filter (\toll -> any (gateWithinBoundingBox boundingBox) toll.tollStartGates) tolls
      case mbTracking of
        Just tracking -> do
          mbTollCombinationsWithStartGatesInPrevBatch :: Maybe [Toll] <-
            Hedis.safeGet (tollStartGateTrackingKey tracking.trackingScope tracking.trackingDriverId)
          case mbTollCombinationsWithStartGatesInPrevBatch of
            Just tollCombinationsWithStartGatesInPrevBatch -> getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide tracking chargedTollIdsInRide previousSegment tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute
            Nothing -> getAggregatedTollCharges chargedTollIdsInRide previousSegment route eligibleTollsThatMaybePresentOnTheRoute emptyTollInfo
        Nothing -> getAggregatedTollCharges chargedTollIdsInRide previousSegment route eligibleTollsThatMaybePresentOnTheRoute emptyTollInfo
    else return Nothing
  where
    getAggregatedTollCharges chargedTollIds mbPreviousSegment remainingRoute eligibleTollsThatMaybePresentOnTheRoute initialTollInfo = do
      aggregatedTollInfo <-
        getAggregatedTollChargesAndNamesOnRoute mbTracking chargedTollIds mbPreviousSegment remainingRoute eligibleTollsThatMaybePresentOnTheRoute initialTollInfo
      if hasDetectedTolls aggregatedTollInfo
        then return $ Just aggregatedTollInfo
        else return Nothing

    getAggregatedTollChargesConsideringStartSegmentFromPrevBatchWhileOnRide tracking chargedTollIds mbPreviousSegment tollCombinationsWithStartGatesInPrevBatch eligibleTollsThatMaybePresentOnTheRoute = do
      case getExitTollAndRemainingRoute route tollCombinationsWithStartGatesInPrevBatch of
        Just (exitSegment, remainingRoute, toll) -> do
          removeMatchedTollFromCache tracking tollCombinationsWithStartGatesInPrevBatch toll
          getAggregatedTollCharges chargedTollIds (Just exitSegment) remainingRoute eligibleTollsThatMaybePresentOnTheRoute (addTollChargeWithinMaxCount chargedTollIds toll emptyTollInfo)
        Nothing -> getAggregatedTollCharges chargedTollIds mbPreviousSegment route eligibleTollsThatMaybePresentOnTheRoute emptyTollInfo

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
    mbToll <- getTollInfoOnRoute merchantOpCityId Nothing Nothing r.points
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
