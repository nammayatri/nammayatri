{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | What keeps a shared-route run covered, as pure decisions over a snapshot of the run ('RunState'): whether to ask
-- the assigned driver to acknowledge, what the fallback job does, who it tries, and what happens when nobody takes the
-- run. The effects live in "SharedLogic.SharedRun.Coverage"; nothing here reads a clock or a table.
--
-- The order the spec asks for is owned fleet, then the curated board, then the control centre: the fallback job tries
-- the drivers of the owned fleet ('selectCandidates'), leaves the run on the scheduled board (where curated drivers see
-- it through the visibility config) and, once the fallback window is reached, hands it to ops ('afterNoAssignment').
module SharedLogic.SharedRun.Coverage.Rules
  ( RunState (..),
    Assignment (..),
    CheckInDecision (..),
    decideCheckIn,
    FallbackDecision (..),
    decideFallback,
    NoAssignmentStep (..),
    afterNoAssignment,
    CandidateSource (..),
    Candidate (..),
    Exclusion (..),
    selectCandidates,
  )
where

import Data.List (nubBy, sortOn)
import qualified Data.Set as Set
import Data.Time (addUTCTime)
import Kernel.Prelude

-- | The run as the coverage jobs see it when they wake up.
data RunState = RunState
  { now :: UTCTime,
    -- | the run's first departure (the booking's start time)
    startTime :: UTCTime,
    -- | a SharedRoute booking that is neither cancelled nor finished
    liveRun :: Bool,
    -- | the active UPCOMING ride, if a driver holds the run
    assignment :: Maybe Assignment,
    -- | when the driver is asked to acknowledge; Nothing when the city never asks
    checkInDue :: Maybe UTCTime,
    -- | when an unacknowledged or uncovered run goes to the fallback
    fallbackDue :: UTCTime
  }
  deriving (Show, Eq)

newtype Assignment = Assignment {acknowledged :: Bool}
  deriving (Show, Eq)

data CheckInDecision
  = -- | push the acknowledgement request to the assigned driver
    AskToAcknowledge
  | SkipCheckIn Text
  deriving (Show, Eq)

-- | At check-in time: ask the driver holding a live run that has not been acknowledged yet. Anything else is a no-op.
decideCheckIn :: RunState -> CheckInDecision
decideCheckIn s
  | not s.liveRun = SkipCheckIn "not a live run"
  | s.now >= s.startTime = SkipCheckIn "the run has departed"
  | isNothing s.checkInDue = SkipCheckIn "check-in is disabled for the city"
  | otherwise = case s.assignment of
    Nothing -> SkipCheckIn "no driver holds the run"
    Just a
      | a.acknowledged -> SkipCheckIn "already acknowledged"
      | otherwise -> AskToAcknowledge

data FallbackDecision
  = -- | nothing to cover any more (cancelled, finished, departed, not a run)
    StopFallback Text
  | -- | the assigned driver did not acknowledge in time: unassign them (the cancel path puts the run back on the board
    -- and runs the fallback again)
    UnassignUnacknowledged
  | -- | a driver holds the run and has acknowledged, or the city does not ask, or it is not yet time to judge
    KeepAssignment
  | -- | nobody holds the run: try the owned fleet
    FindDriver
  deriving (Show, Eq)

-- | The fallback job's first step on a freshly read run.
decideFallback :: RunState -> FallbackDecision
decideFallback s
  | not s.liveRun = StopFallback "not a live run"
  | s.now >= s.startTime = StopFallback "the run has departed"
  | otherwise = case s.assignment of
    Just a
      | isJust s.checkInDue && not a.acknowledged && s.now >= s.fallbackDue -> UnassignUnacknowledged
      | otherwise -> KeepAssignment
    Nothing -> FindDriver

data NoAssignmentStep = NoAssignmentStep
  { -- | the run is uncovered at or after its fallback time: tell ops (at most once per run; the caller guards it)
    alertOps :: Bool,
    -- | when to look again; Nothing: stop (the scheduled fallback, or departure, comes first)
    recheckAt :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | Nobody in the owned fleet took the run; it stays on the board. Before the fallback time the scheduled fallback job
-- still comes, so stop. From then on, tell ops and look once more just before the scheduled-ride buffer closes.
afterNoAssignment :: RunState -> NominalDiffTime -> NoAssignmentStep
afterNoAssignment s scheduleRideBufferTime
  | s.now < s.fallbackDue = NoAssignmentStep {alertOps = False, recheckAt = Nothing}
  | otherwise =
    let finalCheck = addUTCTime (negate scheduleRideBufferTime) s.startTime
     in NoAssignmentStep {alertOps = True, recheckAt = if finalCheck > s.now then Just finalCheck else Nothing}

-- | Bound vehicles first: the driver of a vehicle the route is mapped to, then the other drivers of those fleets.
data CandidateSource = BoundVehicle | FleetDriver
  deriving (Show, Eq, Ord)

-- | A driver the fallback may try, identified by @driverId@, carrying whatever the caller needs to assign them.
data Candidate a p = Candidate
  { driverId :: a,
    source :: CandidateSource,
    -- | the driver's vehicle offers the run's service tier
    servesTier :: Bool,
    payload :: p
  }
  deriving (Show, Eq)

data Exclusion
  = -- | this driver dropped the run before
    DroppedRun
  | -- | the vehicle does not serve the run's service tier
    WrongServiceTier
  deriving (Show, Eq)

-- | Who the fallback tries, in order: bound-vehicle drivers, then fleet drivers, each kept at its first (best) place,
-- minus anyone who dropped this run and anyone whose vehicle cannot serve its tier. Also says who was left out and why.
selectCandidates :: Ord a => [a] -> [Candidate a p] -> ([Candidate a p], [(a, Exclusion)])
selectCandidates dropped candidates =
  let droppedSet = Set.fromList dropped
      ordered = nubBy (\x y -> x.driverId == y.driverId) (sortOn (.source) candidates)
      verdict c
        | c.driverId `Set.member` droppedSet = Left (c.driverId, DroppedRun)
        | not c.servesTier = Left (c.driverId, WrongServiceTier)
        | otherwise = Right c
      verdicts = map verdict ordered
   in ([c | Right c <- verdicts], [e | Left e <- verdicts])
