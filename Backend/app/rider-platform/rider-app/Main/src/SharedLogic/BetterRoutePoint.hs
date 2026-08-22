{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Detects a point already on the search route that is a short walk from the
-- customer's pickup (or drop) but sits far along the route by road — the signature
-- of a detour the ride is forced to make for one-ways, dividers or inner lanes.
--
-- Purely geometric: it works off the polyline 'Kernel.External.Maps.getRoutes' has
-- already returned, so detection costs no extra map API call. The customer's own
-- pickup and drop are never overridden; a hit is surfaced as an additional
-- suggested estimate they can choose.
--
-- Note it measures against the route's /segments/, not its vertices. That is not a
-- refinement — it is what makes this work at all. Route responses are simplified: a
-- 10km OSRM route came back with 28 points in local testing, roughly one every 380m,
-- so the vertex nearest a detour is typically hundreds of metres from the customer
-- even when the road itself passes within 40m. Projecting onto segments makes
-- detection independent of how densely the provider happens to have sampled the line.
module SharedLogic.BetterRoutePoint
  ( BetterPointConfig (..),
    BetterPointKind (..),
    BetterPoint (..),
    BetterRoute (..),
    BetterRoutePlan (..),
    findBetterRoutePoints,
    betterRouteForCustomPoints,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

-- | Thresholds are relative to the ride, with absolute guards. A fixed cap cannot serve
-- both ends of the range: 300m of walking is absurd on a 2km hop and trivial on a 20km
-- one, and a detour worth skipping scales the same way. The percentages decide what is
-- eligible; 'walkAversion' then decides which eligible point wins.
data BetterPointConfig = BetterPointConfig
  { -- | Absolute floor on the saving, whatever the ride length.
    minRideDistanceSaving :: Meters,
    -- | Saving must also be at least this fraction of the ride distance.
    minSavingPctOfRide :: Double,
    -- | Absolute ceiling on the walk, whatever the ride length.
    maxWalkDistance :: Meters,
    -- | Walk is additionally capped at this fraction of the ride distance.
    maxWalkPctOfRide :: Double,
    -- | How many metres of riding one metre of walking is worth when ranking candidates.
    -- 1 treats them equally, which always sends the customer to the walk cap; higher
    -- values prefer a shorter walk. Affects ranking only, never qualification.
    walkAversion :: Double
  }
  deriving (Show, Eq)

-- | Which end (or ends) of the ride a suggestion moves. The customer walks at every end
-- named here, so 'BOTH' asks twice as much of them as the other two.
data BetterPointKind = PICKUP | DROP | BOTH
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data BetterPoint = BetterPoint
  { point :: LatLong,
    -- | Straight-line distance the customer walks to reach 'point'.
    walkDistance :: Meters,
    -- | Ride distance this point cuts out. 'Nothing' when the route was resolved fresh
    -- rather than read off the parent's polyline, since the saving is then only known for
    -- the ride as a whole and cannot be attributed to one end of it.
    rideDistanceSaved :: Maybe Meters
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | One way to reshape the ride: the moved endpoint(s), what it saves, and the route that
-- results. 'betterPickup' and 'betterDrop' are set exactly as 'kind' says.
data BetterRoute = BetterRoute
  { kind :: BetterPointKind,
    betterPickup :: Maybe BetterPoint,
    betterDrop :: Maybe BetterPoint,
    totalRideDistanceSaved :: Meters,
    newRouteDistance :: Meters,
    newRouteDuration :: Maybe Seconds,
    -- | The original polyline trimmed to the suggested endpoints, ready to send
    -- to the BPP as the route for the suggested estimate.
    newRoutePoints :: [LatLong]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | What to offer the customer: one default, plus the other shapes they could pick
-- instead. Only 'best' is worth pricing up front — walking at both ends, or at the end
-- that ranked lower, is a deliberate choice they have to make first.
data BetterRoutePlan = BetterRoutePlan
  { best :: BetterRoute,
    alternatives :: [BetterRoute]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | One segment of the polyline, with how far along the whole line it starts.
data Segment = Segment
  { sIndex :: Int,
    sStart :: LatLong,
    sEnd :: LatLong,
    sLength :: Double,
    sCumAtStart :: Double
  }

-- | A point found on a segment: where it is, how far the customer walks to it, and how
-- much ride distance it saves. 'cPosition' orders candidates along the route.
data Candidate = Candidate
  { cPosition :: (Int, Double),
    -- | Polyline distance from the route start to this point.
    cCum :: Double,
    cPoint :: LatLong,
    cWalk :: Double,
    cSaving :: Double
  }

-- | The polyline measured once: every question this module answers is some reading off
-- these numbers, and recomputing them per candidate shape would be the bulk of the work.
data Geometry = Geometry
  { gPts :: [LatLong],
    gSegments :: [Segment],
    gPolylineLength :: Double,
    -- | Road distance the provider reported, or the polyline length if it reported none.
    gRoadDistance :: Double,
    -- | The polyline is a chain of straight hops, so it measures a little short of the
    -- road distance the provider reports. Multiply polyline distances by this so a saving
    -- is stated in the same units as the distance the customer is quoted on.
    gScale :: Double,
    gDuration :: Maybe Seconds
  }

mkGeometry :: [LatLong] -> Maybe Meters -> Maybe Seconds -> Maybe Geometry
mkGeometry pts mbRouteDistance mbRouteDuration = do
  guard (length pts >= 2)
  let segmentLengths = zipWith straightLine pts (drop 1 pts)
      -- cumulative !! i == polyline distance from the start of the route to pts !! i
      cumulative = scanl (+) 0 segmentLengths
      polylineLength = sum segmentLengths
  guard (polylineLength > 0)
  let roadDistance = maybe polylineLength (fromIntegral . getMeters) mbRouteDistance
      segments =
        zipWith
          (\i ((a, b, len), cum) -> Segment {sIndex = i, sStart = a, sEnd = b, sLength = len, sCumAtStart = cum})
          [0 ..]
          (zip (zip3 pts (drop 1 pts) segmentLengths) cumulative)
  pure
    Geometry
      { gPts = pts,
        gSegments = segments,
        gPolylineLength = polylineLength,
        gRoadDistance = roadDistance,
        gScale = roadDistance / polylineLength,
        gDuration = mbRouteDuration
      }

-- | Walking forward from the pickup saves everything up to that point; mirrored for the drop.
savingAtPickup :: Geometry -> Double -> Double
savingAtPickup g cum = cum * g.gScale

savingAtDrop :: Geometry -> Double -> Double
savingAtDrop g cum = (g.gPolylineLength - cum) * g.gScale

-- | Returns 'Nothing' when the route has no better point worth suggesting.
--
-- Note the criterion is self-validating: a candidate only qualifies when its road
-- distance from the customer (>= 'minRideDistanceSaving') far exceeds the walk to it
-- (<= 'maxWalkDistance'). On a straight road those two are nearly equal, so ordinary
-- routes produce no suggestion at all.
findBetterRoutePoints ::
  BetterPointConfig ->
  -- | The customer's pickup
  LatLong ->
  -- | The customer's drop
  LatLong ->
  -- | Route polyline, pickup first and drop last
  [LatLong] ->
  -- | Road distance the provider reported for the route
  Maybe Meters ->
  -- | Duration the provider reported for the route
  Maybe Seconds ->
  Maybe BetterRoutePlan
findBetterRoutePoints cfg pickup dropPoint pts mbRouteDistance mbRouteDuration = do
  g <- mkGeometry pts mbRouteDistance mbRouteDuration
  -- Scale the thresholds to this ride before using them anywhere.
  let effMaxWalk = min maxWalkCeilingD (cfg.maxWalkPctOfRide * g.gRoadDistance)
      effMinSaving = max minSavingFloorD (cfg.minSavingPctOfRide * g.gRoadDistance)
      -- A segment that ends before the saving threshold can never qualify — start the
      -- scan past it. Same for the drop, mirrored. This is a strict prune of segments
      -- whose every point fails the saving test, not an approximation, and on a dense
      -- provider polyline it drops most of the work.
      mbPickup =
        bestCandidate pickup (savingAtPickup g) effMaxWalk effMinSaving $
          filter (\seg -> (seg.sCumAtStart + seg.sLength) * g.gScale >= effMinSaving) g.gSegments
      mbDrop =
        bestCandidate dropPoint (savingAtDrop g) effMaxWalk effMinSaving $
          filter (\seg -> (g.gPolylineLength - seg.sCumAtStart) * g.gScale >= effMinSaving) g.gSegments
      -- Only a single-sided shape can be the default, so the customer is never quietly
      -- signed up to walk twice: 'BOTH' is offered, never chosen for them. Ranking the
      -- two sides by the same 'routeNetBenefit' also subsumes the crossing case, since one
      -- suggestion alone cannot invert the ride.
      ranked = rankRoutes cfg $ catMaybes [mkRoute g mbPickup Nothing, mkRoute g Nothing mbDrop]
  best <- listToMaybe ranked
  -- Only a genuine two-sided shape: with one side missing 'mkRoute' would hand back the
  -- single-sided route again, and it would be offered as an alternative to itself.
  let mbBoth = case (mbPickup, mbDrop) of
        (Just _, Just _) -> mkRoute g mbPickup mbDrop
        _ -> Nothing
  pure
    BetterRoutePlan
      { best,
        alternatives = rankRoutes cfg (drop 1 ranked <> maybeToList mbBoth)
      }
  where
    maxWalkCeilingD = fromIntegral $ getMeters cfg.maxWalkDistance
    minSavingFloorD = fromIntegral $ getMeters cfg.minRideDistanceSaving

    -- Three candidate points per segment: the two ends of the walkable window, and the
    -- nearest point (the perpendicular foot) clamped into it. Saving is linear along the
    -- segment and walk is convex, so @saving - k*walk@ is concave over the window and its
    -- maximum is at an end or at the foot -- these three bracket it.
    --
    -- Evaluating only the far end, as an earlier version did, meant the short-walk option
    -- was never generated at all, so the weight had nothing better to pick and every
    -- suggestion came back pinned at the walk cap.
    bestCandidate :: LatLong -> (Double -> Double) -> Double -> Double -> [Segment] -> Maybe Candidate
    bestCandidate origin savingOf maxWalkD minSavingD = foldl' step Nothing
      where
        step acc seg =
          case walkWindow origin maxWalkD seg.sStart seg.sEnd of
            Nothing -> acc
            Just (tLo, tFoot, tHi) -> foldl' consider acc [tLo, tFoot, tHi]
          where
            consider inner t =
              let p = interpolate seg.sStart seg.sEnd t
                  -- Same planar metric the window was solved in, so a point the solver
                  -- placed exactly on the boundary cannot then fail the check.
                  walk = planarDistance origin p
                  cum = seg.sCumAtStart + t * seg.sLength
                  saving = savingOf cum
                  candidate = Candidate {cPosition = (seg.sIndex, t), cCum = cum, cPoint = p, cWalk = walk, cSaving = saving}
               in if not (saving >= minSavingD && saving > walk)
                    then inner
                    else case inner of
                      Just best | candidateNetBenefit cfg best >= candidateNetBenefit cfg candidate -> inner
                      _ -> Just candidate

-- | Builds the route for endpoints the customer picked themselves — an alternative they
-- tapped, or a marker they nudged — rather than ones this module proposed. The
-- qualification thresholds deliberately do not apply: they exist to decide what is worth
-- offering unprompted, and the customer has already decided here.
--
-- Each supplied point is projected onto the polyline to find how much of the route it
-- trims. A point further than @maxOffRouteDistance@ from every segment cannot be read off
-- this route at all, and yields 'Nothing' so the caller can resolve a fresh one instead.
betterRouteForCustomPoints ::
  -- | How far off the polyline a supplied point may sit and still be read off it
  Meters ->
  -- | The customer's own pickup, which the walk is measured from
  LatLong ->
  -- | The customer's own drop, which the walk is measured to
  LatLong ->
  -- | Route polyline, pickup first and drop last
  [LatLong] ->
  Maybe Meters ->
  Maybe Seconds ->
  -- | Chosen pickup, when that end moves
  Maybe LatLong ->
  -- | Chosen drop, when that end moves
  Maybe LatLong ->
  Maybe BetterRoute
betterRouteForCustomPoints maxOffRouteDistance pickup dropPoint pts mbRouteDistance mbRouteDuration mbChosenPickup mbChosenDrop = do
  guard (isJust mbChosenPickup || isJust mbChosenDrop)
  g <- mkGeometry pts mbRouteDistance mbRouteDuration
  let maxOffRouteD = fromIntegral $ getMeters maxOffRouteDistance
  pickupCand <- traverse (snapCandidate g (savingAtPickup g) pickup maxOffRouteD) mbChosenPickup
  dropCand <- traverse (snapCandidate g (savingAtDrop g) dropPoint maxOffRouteD) mbChosenDrop
  mkRoute g pickupCand dropCand

-- | Where a customer-chosen point sits along the route. The candidate keeps the point the
-- customer actually chose, not its projection: the projection only answers "how much of
-- the polyline does this cut", and moving them onto the line would relocate the pickup
-- they picked.
snapCandidate :: Geometry -> (Double -> Double) -> LatLong -> Double -> LatLong -> Maybe Candidate
snapCandidate g savingOf ownPoint maxOffRouteD chosen = do
  (seg, t, offRoute) <- foldl' nearer Nothing g.gSegments
  guard (offRoute <= maxOffRouteD)
  let cum = seg.sCumAtStart + t * seg.sLength
  pure
    Candidate
      { cPosition = (seg.sIndex, t),
        cCum = cum,
        cPoint = chosen,
        cWalk = planarDistance ownPoint chosen,
        cSaving = savingOf cum
      }
  where
    nearer acc seg =
      let t = footParam chosen seg.sStart seg.sEnd
          d = planarDistance chosen (interpolate seg.sStart seg.sEnd t)
       in case acc of
            Just (_, _, bestD) | bestD <= d -> acc
            _ -> Just (seg, t, d)

-- | Assembles a route from the endpoints that move. 'Nothing' when the combination is not
-- a ride: no endpoint moved, the pickup landed past the drop, or nothing is left to ride.
mkRoute :: Geometry -> Maybe Candidate -> Maybe Candidate -> Maybe BetterRoute
mkRoute g pickupCand dropCand = do
  kind <- case (pickupCand, dropCand) of
    (Just _, Just _) -> Just BOTH
    (Just _, Nothing) -> Just PICKUP
    (Nothing, Just _) -> Just DROP
    (Nothing, Nothing) -> Nothing
  case (pickupCand, dropCand) of
    (Just p, Just d) -> guard (p.cCum < d.cCum)
    _ -> pure ()
  let savedPolyline = maybe 0 (.cCum) pickupCand + maybe 0 ((g.gPolylineLength -) . (.cCum)) dropCand
      savedRoad = savedPolyline * g.gScale
      remainingRoad = g.gRoadDistance - savedRoad
  guard (remainingRoad > 0)
  let newPoints = trimmedPoints g pickupCand dropCand
  guard (length newPoints >= 2)
  pure
    BetterRoute
      { kind,
        betterPickup = toBetterPoint <$> pickupCand,
        betterDrop = toBetterPoint <$> dropCand,
        totalRideDistanceSaved = toMeters savedRoad,
        newRouteDistance = toMeters remainingRoad,
        -- Proportional: a trimmed detour is usually slower per metre than the rest of
        -- the route, so this slightly under-estimates the time saved. The BPP recomputes
        -- fare from the distance, so this only affects the displayed ETA.
        newRouteDuration = g.gDuration <&> \d -> Seconds . round $ fromIntegral (getSeconds d) * (remainingRoad / g.gRoadDistance),
        newRoutePoints = newPoints
      }
  where
    toBetterPoint c =
      BetterPoint
        { point = c.cPoint,
          walkDistance = toMeters c.cWalk,
          rideDistanceSaved = Just $ toMeters c.cSaving
        }

-- | The suggested endpoints sit mid-segment, so they replace the vertices they cut
-- past rather than being appended to them.
trimmedPoints :: Geometry -> Maybe Candidate -> Maybe Candidate -> [LatLong]
trimmedPoints g pickupCand dropCand =
  let pts = g.gPts
      n = length pts
      startPt = maybe (take 1 pts) (pure . (.cPoint)) pickupCand
      endPt = maybe (drop (n - 1) pts) (pure . (.cPoint)) dropCand
      -- Vertices strictly between the two projected points.
      startIdx = maybe 1 (\c -> fst c.cPosition + 1) pickupCand
      endIdx = maybe (n - 2) (\c -> fst c.cPosition) dropCand
      middle = take (endIdx - startIdx + 1) (drop startIdx pts)
   in startPt <> middle <> endPt

rankRoutes :: BetterPointConfig -> [BetterRoute] -> [BetterRoute]
rankRoutes cfg = sortOn (Down . routeNetBenefit cfg)

-- | Metres of riding saved, discounted by what the walk is worth in the same units.
routeNetBenefit :: BetterPointConfig -> BetterRoute -> Double
routeNetBenefit cfg route =
  fromIntegral (getMeters route.totalRideDistanceSaved) - cfg.walkAversion * totalWalk
  where
    totalWalk = sum $ map (fromIntegral . getMeters . (.walkDistance)) (catMaybes [route.betterPickup, route.betterDrop])

candidateNetBenefit :: BetterPointConfig -> Candidate -> Double
candidateNetBenefit cfg c = c.cSaving - cfg.walkAversion * c.cWalk

straightLine :: LatLong -> LatLong -> Double
straightLine a b = realToFrac $ distanceBetweenInMeters a b

toMeters :: Double -> Meters
toMeters = Meters . round

-- | Local flat-earth projection centred on @origin@ -- exact enough over a walk radius,
-- and cheap enough to run per segment.
localXY :: LatLong -> LatLong -> (Double, Double)
localXY origin p = ((p.lon - origin.lon) * metresPerDegLon, (p.lat - origin.lat) * metresPerDegLat)
  where
    metresPerDegLat = 111132.0
    metresPerDegLon = 111320.0 * cos (origin.lat * pi / 180)

planarDistance :: LatLong -> LatLong -> Double
planarDistance origin p = let (x, y) = localXY origin p in sqrt (x * x + y * y)

-- | Where along segment @a -> b@ the point nearest @origin@ lies, as a fraction clamped
-- into the segment.
footParam :: LatLong -> LatLong -> LatLong -> Double
footParam origin a b
  | lenSq <= 0 = 0
  | otherwise = max 0 (min 1 (- aDotD / lenSq))
  where
    (ax, ay) = localXY origin a
    (bx, by) = localXY origin b
    (dx, dy) = (bx - ax, by - ay)
    lenSq = dx * dx + dy * dy
    aDotD = ax * dx + ay * dy

-- | The stretch of segment @a -> b@ within @radius@ of @origin@ as @(lo, foot, hi)@ --
-- the window ends plus the nearest point clamped into it -- or 'Nothing' if the segment
-- never comes that close.
--
-- Solved in closed form rather than by stepping along the line: @|A + tD| <= r@ is a
-- quadratic in @t@, so the walkable stretch is exactly the interval between its roots.
-- That avoids inventing a step size, and is exact at the interval ends -- which is
-- where the best candidate always sits.
walkWindow :: LatLong -> Double -> LatLong -> LatLong -> Maybe (Double, Double, Double)
walkWindow origin radius a b
  | lenSq <= 0 = if c <= 0 then Just (0, 0, 0) else Nothing
  | disc < 0 = Nothing
  | t2 < 0 || t1 > 1 = Nothing
  | otherwise = Just (clamp t1, clampTo (clamp t1) (clamp t2) foot, clamp t2)
  where
    (ax, ay) = localXY origin a
    (bx, by) = localXY origin b
    (dx, dy) = (bx - ax, by - ay)
    lenSq = dx * dx + dy * dy
    aDotD = ax * dx + ay * dy
    c = ax * ax + ay * ay - radius * radius
    disc = aDotD * aDotD - lenSq * c
    root = sqrt disc
    t1 = (- aDotD - root) / lenSq
    t2 = (- aDotD + root) / lenSq
    clamp t = max 0 (min 1 t)
    foot = - aDotD / lenSq
    clampTo lo hi t = max lo (min hi t)

interpolate :: LatLong -> LatLong -> Double -> LatLong
interpolate a b t = LatLong (a.lat + t * (b.lat - a.lat)) (a.lon + t * (b.lon - a.lon))
