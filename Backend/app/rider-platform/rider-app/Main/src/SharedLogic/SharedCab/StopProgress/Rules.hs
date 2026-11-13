-- | Stop-progress decisions, pure: the tick reads LTS and Redis, asks these, and acts.
module SharedLogic.SharedCab.StopProgress.Rules
  ( StopMark (..),
    CabFix (..),
    DropAction (..),
    OffRouteAction (..),
    boardStopPassed,
    armMovingTimer,
    dropAction,
    reachedDropStop,
    offRouteAction,
  )
where

import Data.Time (addUTCTime, diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
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
    stops :: [StopMark],
    -- | gps-processor's `is_on_stop` for the cab's first unreached stop
    onStop :: Maybe Bool
  }
  deriving (Show, Eq)

data DropAction = StartDropClock | AutoEnd | KeepWaiting
  deriving (Show, Eq)

-- | `AskCheckIn` carries the pause's deadline.
data OffRouteAction = StartOffRouteClock | ClearOffRouteClock | AskCheckIn UTCTime | PauseOffRoute | OffRouteNoChange
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
-- "Gets to" is gps-processor holding the cab at it when `is_on_stop` is sent, else being within atStopRadiusM. Never on an ETA: the
-- timer's expiry blames the rider, which is fair only once the cab is at the stop.
armMovingTimer :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> Maybe UTCTime
armMovingTimer cfg now expiresAt freshCab boardStop
  | isJust expiresAt = Nothing
  | any atBoardStop freshCab = Just (addUTCTime (fromIntegral cfg.movingTimerSec) now)
  | otherwise = Nothing
  where
    atBoardStop cab = case cab.onStop of
      Just held -> held && ((.stopCode) <$> find (not . (.reached)) cab.stops) == Just boardStop
      Nothing -> any (\s -> s.stopCode == boardStop && not s.reached && meters cab.position s.coordinate <= fromIntegral cfg.atStopRadiusM) cab.stops

-- | The clock starts when the cab gets to the drop stop and ends the ride `autoEndAfterDropSec` later, wherever the cab is by then.
dropAction :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> DropAction
dropAction cfg now clock mbFix dropStop = case clock of
  Just since
    | diffUTCTime now since >= fromIntegral cfg.autoEndAfterDropSec -> AutoEnd
    | otherwise -> KeepWaiting
  Nothing
    | any (\cab -> reachedDropStop cfg cab dropStop) mbFix -> StartDropClock
    | otherwise -> KeepWaiting

-- | The cab has got to the drop stop: passed it, or within atStopRadiusM of it (the route's last stop is never marked reached).
reachedDropStop :: Config.SharedCabTunables -> CabFix -> Text -> Bool
reachedDropStop cfg cab dropStop = any (\s -> s.stopCode == dropStop && arrivedAt (fromIntegral cfg.atStopRadiusM) cab s) cab.stops

-- | gps-processor flags a fresh fix `off_route`; flagged for `offRouteSec` straight pauses the session, and the driver is asked to
-- confirm the route `checkInLeadSec` before that. No fresh fix decides nothing, so the clock is neither started nor cleared.
offRouteAction :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe Bool -> OffRouteAction
offRouteAction cfg now offSince mbOffRoute = case (mbOffRoute, offSince) of
  (Just True, Nothing) -> StartOffRouteClock
  (Just True, Just since)
    | elapsed since >= fromIntegral cfg.offRouteSec -> PauseOffRoute
    | elapsed since >= fromIntegral (cfg.offRouteSec - cfg.checkInLeadSec) -> AskCheckIn (addUTCTime (fromIntegral cfg.offRouteSec) since)
    | otherwise -> OffRouteNoChange
  (Just False, Just _) -> ClearOffRouteClock
  _ -> OffRouteNoChange
  where
    elapsed = diffUTCTime now
