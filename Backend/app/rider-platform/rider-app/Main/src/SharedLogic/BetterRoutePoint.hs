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
    BetterPoint (..),
    BetterRoute (..),
    findBetterRoutePoints,
  )
where

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

data BetterPoint = BetterPoint
  { point :: LatLong,
    -- | Straight-line distance the customer walks to reach 'point'.
    walkDistance :: Meters,
    -- | Ride distance this point cuts out.
    rideDistanceSaved :: Meters
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | At most one of 'betterPickup' / 'betterDrop' is ever set: only the end with the
-- better saving-per-walk trade-off is suggested, so the customer never walks at both ends.
data BetterRoute = BetterRoute
  { betterPickup :: Maybe BetterPoint,
    betterDrop :: Maybe BetterPoint,
    totalRideDistanceSaved :: Meters,
    newRouteDistance :: Meters,
    newRouteDuration :: Maybe Seconds,
    -- | The original polyline trimmed to the suggested endpoints, ready to send
    -- to the BPP as the route for the suggested estimate.
    newRoutePoints :: [LatLong]
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
  Maybe BetterRoute
findBetterRoutePoints cfg pickup dropPoint pts mbRouteDistance mbRouteDuration = do
  guard (length pts >= 2)
  let segmentLengths = zipWith straightLine pts (drop 1 pts)
      -- cumulative !! i == polyline distance from the start of the route to pts !! i
      cumulative = scanl (+) 0 segmentLengths
      polylineLength = sum segmentLengths
  guard (polylineLength > 0)
  let roadDistance = maybe polylineLength (fromIntegral . getMeters) mbRouteDistance
      -- The polyline is a chain of straight hops, so it measures a little short of
      -- the road distance the provider reports. Rescale so that a saving is stated in
      -- the same units as the distance the customer is quoted on.
      scale = roadDistance / polylineLength
      segments =
        zipWith
          (\i ((a, b, len), cum) -> Segment {sIndex = i, sStart = a, sEnd = b, sLength = len, sCumAtStart = cum})
          [0 ..]
          (zip (zip3 pts (drop 1 pts) segmentLengths) cumulative)
      -- Walking forward from the pickup saves everything up to that point, so a segment
      -- that ends before the threshold can never qualify — start the scan past it. Same
      -- for the drop, mirrored. This is a strict prune of segments whose every point
      -- fails the saving test, not an approximation, and on a dense provider polyline it
      -- drops most of the work.
      -- Scale the thresholds to this ride before using them anywhere.
      effMaxWalk = min maxWalkCeilingD (cfg.maxWalkPctOfRide * roadDistance)
      effMinSaving = max minSavingFloorD (cfg.minSavingPctOfRide * roadDistance)
      mbPickup =
        bestCandidate pickup (\cum -> cum * scale) effMaxWalk effMinSaving $
          filter (\seg -> (seg.sCumAtStart + seg.sLength) * scale >= effMinSaving) segments
      mbDrop =
        bestCandidate dropPoint (\cum -> (polylineLength - cum) * scale) effMaxWalk effMinSaving $
          filter (\seg -> (polylineLength - seg.sCumAtStart) * scale >= effMinSaving) segments
  -- At most one end is ever suggested, so the customer walks once: the two are ranked
  -- by the same 'netBenefit' used within a side, so a short walk can beat a longer one
  -- that saves a little more. This also subsumes the crossing case, since two
  -- suggestions can no longer invert the ride between them.
  let (pickupCand, dropCand) = keepBetterSide mbPickup mbDrop
  guard (isJust pickupCand || isJust dropCand)
  let savedPolyline = maybe 0 cCum pickupCand + maybe 0 ((polylineLength -) . cCum) dropCand
      savedRoad = savedPolyline * scale
      remainingRoad = roadDistance - savedRoad
  guard (remainingRoad > 0)
  let newPoints = trimmedPoints pickupCand dropCand
  guard (length newPoints >= 2)
  pure
    BetterRoute
      { betterPickup = toBetterPoint <$> pickupCand,
        betterDrop = toBetterPoint <$> dropCand,
        totalRideDistanceSaved = toMeters savedRoad,
        newRouteDistance = toMeters remainingRoad,
        -- Proportional: a trimmed detour is usually slower per metre than the rest of
        -- the route, so this slightly under-estimates the time saved. The BPP recomputes
        -- fare from the distance, so this only affects the displayed ETA.
        newRouteDuration = mbRouteDuration <&> \d -> Seconds . round $ fromIntegral (getSeconds d) * (remainingRoad / roadDistance),
        newRoutePoints = newPoints
      }
  where
    straightLine :: LatLong -> LatLong -> Double
    straightLine a b = realToFrac $ distanceBetweenInMeters a b

    maxWalkCeilingD = fromIntegral $ getMeters cfg.maxWalkDistance
    minSavingFloorD = fromIntegral $ getMeters cfg.minRideDistanceSaving

    toMeters :: Double -> Meters
    toMeters = Meters . round

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
                      Just best | netBenefit best >= netBenefit candidate -> inner
                      _ -> Just candidate

    netBenefit :: Candidate -> Double
    netBenefit c = c.cSaving - walkAversion * c.cWalk

    walkAversion = cfg.walkAversion

    keepBetterSide :: Maybe Candidate -> Maybe Candidate -> (Maybe Candidate, Maybe Candidate)
    keepBetterSide (Just p) (Just d)
      | netBenefit p >= netBenefit d = (Just p, Nothing)
      | otherwise = (Nothing, Just d)
    keepBetterSide p d = (p, d)

    toBetterPoint :: Candidate -> BetterPoint
    toBetterPoint c =
      BetterPoint
        { point = c.cPoint,
          walkDistance = toMeters c.cWalk,
          rideDistanceSaved = toMeters c.cSaving
        }

    -- The suggested endpoints sit mid-segment, so they replace the vertices they cut
    -- past rather than being appended to them.
    trimmedPoints :: Maybe Candidate -> Maybe Candidate -> [LatLong]
    trimmedPoints pickupCand dropCand =
      let n = length pts
          startPt = maybe (take 1 pts) (pure . cPoint) pickupCand
          endPt = maybe (drop (n - 1) pts) (pure . cPoint) dropCand
          -- Vertices strictly between the two projected points.
          startIdx = maybe 1 (\c -> fst c.cPosition + 1) pickupCand
          endIdx = maybe (n - 2) (\c -> fst c.cPosition) dropCand
          middle = take (endIdx - startIdx + 1) (drop startIdx pts)
       in startPt <> middle <> endPt

-- | Local flat-earth projection centred on @origin@ -- exact enough over a walk radius,
-- and cheap enough to run per segment.
localXY :: LatLong -> LatLong -> (Double, Double)
localXY origin p = ((p.lon - origin.lon) * metresPerDegLon, (p.lat - origin.lat) * metresPerDegLat)
  where
    metresPerDegLat = 111132.0
    metresPerDegLon = 111320.0 * cos (origin.lat * pi / 180)

planarDistance :: LatLong -> LatLong -> Double
planarDistance origin p = let (x, y) = localXY origin p in sqrt (x * x + y * y)

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
