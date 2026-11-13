-- | Stop-progress decisions, pure: the tick reads LTS and Redis, asks these, and acts.
module SharedLogic.SharedCab.StopProgress.Rules
  ( StopMark (..),
    CabFix (..),
    DropAction (..),
    OffRouteAction (..),
    boardStopPassed,
    armMovingTimer,
    approachUpdate,
    dropAction,
    offRouteAction,
  )
where

import Data.Time (addUTCTime, diffUTCTime)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import SharedLogic.SharedCab.Allocation.Types (AllocationState (..), ApproachPush (..), ApproachStage (..), approachPushStage, approachStageSent, isOffered)
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

-- | The tick's approach pushes for an allocated rider, from a fresh fix: "arriving soon" once the cab is within arrivingSoonRadiusM of
-- the board stop (still ahead), "on the way" once it is onTheWayM from where it was first seen after the rider was told of it. Only
-- the highest stage reached is pushed, and only above the one the rider already has, so a fast cab never sends two at once, GPS
-- jitter never repeats one, and nothing goes after "arrived". The new record to write and the push due; Nothing writes nothing.
approachUpdate :: Config.SharedCabTunables -> CabFix -> Text -> AllocationState -> Maybe (AllocationState, Maybe ApproachPush)
approachUpdate cfg cab boardStop st
  | isOffered st || sent >= Just ApproachArrivingSoon = Nothing
  | Just p <- due = Just (st {approachStage = Just (approachPushStage p), approachFrom = Just from}, Just p)
  | isNothing st.approachFrom = Just (st {approachFrom = Just from}, Nothing)
  | otherwise = Nothing
  where
    sent = approachStageSent st
    from = fromMaybe cab.position st.approachFrom
    due = mfilter ((> sent) . Just . approachPushStage) reached
    reached
      | any nearBoardStop cab.stops = Just PushArrivingSoon
      | meters from cab.position >= fromIntegral cfg.onTheWayM = Just PushOnTheWay
      | otherwise = Nothing
    nearBoardStop s = s.stopCode == boardStop && not s.reached && meters cab.position s.coordinate <= fromIntegral cfg.arrivingSoonRadiusM

-- | The clock starts when the cab gets to the drop stop and ends the ride `autoEndAfterDropSec` later, wherever the cab is by then.
dropAction :: Config.SharedCabTunables -> UTCTime -> Maybe UTCTime -> Maybe CabFix -> Text -> DropAction
dropAction cfg now clock mbFix dropStop = case clock of
  Just since
    | diffUTCTime now since >= fromIntegral cfg.autoEndAfterDropSec -> AutoEnd
    | otherwise -> KeepWaiting
  Nothing
    | any (\cab -> any (\s -> s.stopCode == dropStop && arrivedAt (fromIntegral cfg.atStopRadiusM) cab s) cab.stops) mbFix -> StartDropClock
    | otherwise -> KeepWaiting

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
