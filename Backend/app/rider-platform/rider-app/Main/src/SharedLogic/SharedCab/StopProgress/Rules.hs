-- | `05` §6 stop-progress decisions, pure: the tick (SharedLogic.SharedCab.StopProgress) reads LTS and Redis, asks these, and acts.
module SharedLogic.SharedCab.StopProgress.Rules
  ( StopProgressConfig (..),
    StopMark (..),
    CabFix (..),
    DropAction (..),
    OffRouteAction (..),
    boardStopPassed,
    armMovingTimer,
    dropAction,
    offRouteAction,
    reachedRouteEnd,
    metersFromPolyline,
  )
where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Time (addUTCTime, diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)

data StopProgressConfig = StopProgressConfig
  { atStopRadiusM :: Double,
    autoEndAfterDropSec :: Int,
    offRouteMeters :: Double,
    offRouteSec :: Int,
    movingTimerSec :: Int
  }
  deriving (Show, Eq)

-- | One stop of the cab's LTS stop list; `reached` = LTS marked it Reached.
data StopMark = StopMark
  { stopCode :: Text,
    stopIdx :: Int,
    reached :: Bool,
    coordinate :: LatLong
  }
  deriving (Show, Eq)

-- | The cab's last LTS entry, fresh or not: Reached marks are history and stay true.
data CabFix = CabFix
  { position :: LatLong,
    stops :: [StopMark]
  }
  deriving (Show, Eq)

data DropAction = StartDropClock | AutoEnd | KeepWaiting
  deriving (Show, Eq)

data OffRouteAction = StartOffRouteClock | ClearOffRouteClock | PauseOffRoute | OffRouteNoChange
  deriving (Show, Eq)

meters :: LatLong -> LatLong -> Double
meters a b = realToFrac (distanceBetweenInMeters a b)

-- | Reached only: a cab standing at the board stop keeps it Upcoming, and that rider can still board.
boardStopPassed :: Text -> CabFix -> Bool
boardStopPassed code cab = any (\s -> s.stopCode == code && s.reached) cab.stops

-- | LTS never marks a route's last stop Reached (nothing lies beyond it to project onto), so arriving within the radius counts too.
arrivedAt :: Double -> CabFix -> StopMark -> Bool
arrivedAt radius cab s = s.reached || meters cab.position s.coordinate <= radius

-- | `05` §2/§6.4: the moving timer starts when the cab gets to the board stop, only if no timer runs yet
-- (a stand timer is cleared once the cab moves) and the stop is still Upcoming (Reached is the passed-stop release).
-- Needs a fresh fix: a stale one parked near the stop would arm it falsely.
armMovingTimer :: StopProgressConfig -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> Maybe UTCTime
armMovingTimer cfg now expiresAt freshCab boardStop
  | isJust expiresAt = Nothing
  | any (\cab -> any (\s -> s.stopCode == boardStop && not s.reached && meters cab.position s.coordinate <= cfg.atStopRadiusM) cab.stops) freshCab =
    Just (addUTCTime (fromIntegral cfg.movingTimerSec) now)
  | otherwise = Nothing

-- | `05` §6.3: the clock starts when the cab gets to the drop stop and ends the ride `autoEndAfterDropSec` later, wherever the cab is by then.
dropAction :: StopProgressConfig -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> DropAction
dropAction cfg now clock mbFix dropStop = case clock of
  Just since
    | diffUTCTime now since >= fromIntegral cfg.autoEndAfterDropSec -> AutoEnd
    | otherwise -> KeepWaiting
  Nothing
    | any (\cab -> any (\s -> s.stopCode == dropStop && arrivedAt cfg.atStopRadiusM cab s) cab.stops) mbFix -> StartDropClock
    | otherwise -> KeepWaiting

-- | `04` §7 (D9): off the polyline by more than `offRouteMeters` for `offRouteSec` pauses the session.
-- No fresh position or no polyline decides nothing, so the clock is neither started nor cleared.
offRouteAction :: StopProgressConfig -> UTCTime -> Maybe UTCTime -> [LatLong] -> Maybe LatLong -> OffRouteAction
offRouteAction cfg now offSince route mbPosition =
  case (mbPosition >>= metersFromPolyline route, offSince) of
    (Just d, Nothing) | d > cfg.offRouteMeters -> StartOffRouteClock
    (Just d, Just since)
      | d > cfg.offRouteMeters -> if diffUTCTime now since >= fromIntegral cfg.offRouteSec then PauseOffRoute else OffRouteNoChange
      | otherwise -> ClearOffRouteClock
    _ -> OffRouteNoChange

-- | The trip's `reachedEndAt`: the route's last stop (highest index in the LTS list) reached.
reachedRouteEnd :: Double -> CabFix -> Bool
reachedRouteEnd radius cab = case sortOn (Down . (.stopIdx)) cab.stops of
  lastStop : _ -> arrivedAt radius cab lastStop
  [] -> False

-- | Shortest distance to any segment, on a local flat projection around the point (routes are a few km).
metersFromPolyline :: [LatLong] -> LatLong -> Maybe Double
metersFromPolyline route p = case route of
  [] -> Nothing
  [only] -> Just (meters p only)
  _ -> Just . minimum $ zipWith segment route (drop 1 route)
  where
    earthR = 6371000
    toXY q = ((q.lon - p.lon) * cos (p.lat * pi / 180) * earthR * pi / 180, (q.lat - p.lat) * earthR * pi / 180)
    segment a b =
      let (ax, ay) = toXY a
          (bx, by) = toXY b
          (dx, dy) = (bx - ax, by - ay)
          len2 = dx * dx + dy * dy
          t = if len2 == 0 then 0 else max 0 (min 1 (- (ax * dx + ay * dy) / len2))
          (cx, cy) = (ax + t * dx, ay + t * dy)
       in sqrt (cx * cx + cy * cy)
