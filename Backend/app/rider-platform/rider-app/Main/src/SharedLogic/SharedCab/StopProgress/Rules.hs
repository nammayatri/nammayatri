-- | Stop-progress decisions, pure: the tick reads LTS and Redis, asks these, and acts.
module SharedLogic.SharedCab.StopProgress.Rules
  ( StopMark (..),
    CabFix (..),
    DropAction (..),
    OffRouteAction (..),
    boardStopPassed,
    armMovingTimer,
    dropAction,
    offRouteAction,
    metersFromPolyline,
  )
where

import Data.Time (addUTCTime, diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Lib.GateInfo.Geometry (pointToSegmentMeters)
import qualified SharedLogic.SharedCab.Config as Config

data StopMark = StopMark
  { stopCode :: Text,
    stopIdx :: Int,
    reached :: Bool,
    coordinate :: LatLong
  }
  deriving (Show, Eq)

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

boardStopPassed :: Text -> CabFix -> Bool
boardStopPassed code cab = any (\s -> s.stopCode == code && s.reached) cab.stops

-- | LTS never marks a route's last stop Reached (nothing lies beyond it to project onto), so arriving within the radius counts too.
arrivedAt :: Double -> CabFix -> StopMark -> Bool
arrivedAt radius cab s = s.reached || meters cab.position s.coordinate <= radius

-- | The moving timer starts when the cab gets to the board stop, only if no timer runs yet (a stand timer is cleared once
-- the cab moves) and the stop is still Upcoming. Needs a fresh fix: a stale one parked near the stop would arm it falsely.
armMovingTimer :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> Maybe UTCTime
armMovingTimer cfg now expiresAt freshCab boardStop
  | isJust expiresAt = Nothing
  | any (\cab -> any (\s -> s.stopCode == boardStop && not s.reached && meters cab.position s.coordinate <= fromIntegral cfg.atStopRadiusM) cab.stops) freshCab =
    Just (addUTCTime (fromIntegral cfg.movingTimerSec) now)
  | otherwise = Nothing

-- | The clock starts when the cab gets to the drop stop and ends the ride `autoEndAfterDropSec` later, wherever the cab is by then.
dropAction :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> DropAction
dropAction cfg now clock mbFix dropStop = case clock of
  Just since
    | diffUTCTime now since >= fromIntegral cfg.autoEndAfterDropSec -> AutoEnd
    | otherwise -> KeepWaiting
  Nothing
    | any (\cab -> any (\s -> s.stopCode == dropStop && arrivedAt (fromIntegral cfg.atStopRadiusM) cab s) cab.stops) mbFix -> StartDropClock
    | otherwise -> KeepWaiting

-- | Off the polyline by more than `offRouteMeters` for `offRouteSec` pauses the session. No fresh position or no polyline
-- decides nothing, so the clock is neither started nor cleared.
offRouteAction :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> [LatLong] -> Maybe LatLong -> OffRouteAction
offRouteAction cfg now offSince route mbPosition =
  case (mbPosition >>= metersFromPolyline route, offSince) of
    (Just d, Nothing) | d > fromIntegral cfg.offRouteMeters -> StartOffRouteClock
    (Just d, Just since)
      | d > fromIntegral cfg.offRouteMeters -> if diffUTCTime now since >= fromIntegral cfg.offRouteSec then PauseOffRoute else OffRouteNoChange
      | otherwise -> ClearOffRouteClock
    _ -> OffRouteNoChange

metersFromPolyline :: [LatLong] -> LatLong -> Maybe Double
metersFromPolyline route p = case route of
  [] -> Nothing
  [only] -> Just (meters p only)
  _ -> Just . minimum $ zipWith (pointToSegmentMeters p) route (drop 1 route)
