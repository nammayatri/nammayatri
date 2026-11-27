{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | What keeps a shared-route run covered: the check-in, the fallback's decision, who it tries and when ops hear of it.
module SharedRunCoverageTests (tests) where

import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "dynamic-offer-driver-app" SharedLogic.ScheduledBooking.OverlapCheck (CommittedInterval (..), DriverClass (..), ScheduledCandidate (..), classifyDriver)
import "dynamic-offer-driver-app" SharedLogic.SharedRun.Coverage.Rules
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- | the run leaves at 07:30 UTC on 2026-11-25
departure :: UTCTime
departure = UTCTime (fromGregorian 2026 11 25) (7 * 3600 + 30 * 60)

minutesBefore :: NominalDiffTime -> UTCTime
minutesBefore m = addUTCTime (negate (m * 60)) departure

-- | check-in 60 minutes before departure, fallback 30 minutes before; the clock reads @at@
run :: UTCTime -> Maybe Assignment -> RunState
run at assignment =
  RunState
    { now = at,
      startTime = departure,
      liveRun = True,
      assignment,
      checkInDue = Just (minutesBefore 60),
      fallbackDue = minutesBefore 30
    }

unacked, acked :: Maybe Assignment
unacked = Just (Assignment {acknowledged = False})
acked = Just (Assignment {acknowledged = True})

here :: LatLong
here = LatLong 12.97 77.59

-- | a 30-minute hold ending @gapMinutes@ before departure
holdEndingBefore :: NominalDiffTime -> Bool -> CommittedInterval
holdEndingBefore gapMinutes isRun =
  CommittedInterval
    { intervalStart = minutesBefore (gapMinutes + 30),
      intervalEnd = minutesBefore gapMinutes,
      intervalPickup = here,
      intervalDrop = here,
      intervalIsRun = isRun
    }

runCandidate :: Bool -> ScheduledCandidate
runCandidate isRun = ScheduledCandidate {candidateStart = departure, candidateEnd = addUTCTime 1800 departure, candidatePickup = here, candidateDrop = Just here, candidateIsRun = isRun}

-- | the city's scheduled-ride buffer is 10 minutes and its clash window 45: the gap 'gapBetween' would give
gapTo :: Bool -> Bool -> NominalDiffTime
gapTo candidateIsRun neighbourIsRun = if candidateIsRun || neighbourIsRun then 45 * 60 else 10 * 60

rejectedOnTime :: DriverClass () -> Bool
rejectedOnTime (ClassDecided _ False) = True
rejectedOnTime _ = False

candidate :: String -> CandidateSource -> Bool -> Candidate String ()
candidate driverId source servesTier = Candidate {driverId, source, servesTier, payload = ()}

tests :: TestTree
tests =
  testGroup
    "shared run coverage"
    [ testGroup
        "check-in"
        [ testCase "an unacknowledged driver is asked" $
            decideCheckIn (run (minutesBefore 60) unacked) @?= AskToAcknowledge,
          testCase "an acknowledged driver is not asked again" $
            decideCheckIn (run (minutesBefore 60) acked) @?= SkipCheckIn "already acknowledged",
          testCase "nobody to ask on an unassigned run" $
            decideCheckIn (run (minutesBefore 60) Nothing) @?= SkipCheckIn "no driver holds the run",
          testCase "a city that never asks skips it" $
            decideCheckIn ((run (minutesBefore 60) unacked) {checkInDue = Nothing}) @?= SkipCheckIn "check-in is disabled for the city",
          testCase "a cancelled run is skipped" $
            decideCheckIn ((run (minutesBefore 60) unacked) {liveRun = False}) @?= SkipCheckIn "not a live run",
          testCase "a departed run is skipped" $
            decideCheckIn (run departure unacked) @?= SkipCheckIn "the run has departed"
        ],
      testGroup
        "fallback"
        [ testCase "unacknowledged at the fallback time: unassign" $
            decideFallback (run (minutesBefore 30) unacked) @?= UnassignUnacknowledged,
          testCase "unacknowledged before the fallback time (driver dropped, job ran early): keep" $
            decideFallback (run (minutesBefore 45) unacked) @?= KeepAssignment,
          testCase "acknowledged: keep" $
            decideFallback (run (minutesBefore 30) acked) @?= KeepAssignment,
          testCase "check-in disabled: an assigned run is kept without acknowledgement" $
            decideFallback ((run (minutesBefore 30) unacked) {checkInDue = Nothing}) @?= KeepAssignment,
          testCase "unassigned: find a driver" $
            decideFallback (run (minutesBefore 90) Nothing) @?= FindDriver,
          testCase "cancelled or finished: stop" $
            decideFallback ((run (minutesBefore 30) Nothing) {liveRun = False}) @?= StopFallback "not a live run",
          testCase "departed: stop" $
            decideFallback (run (addUTCTime 60 departure) Nothing) @?= StopFallback "the run has departed"
        ],
      testGroup
        "nobody took the run"
        [ testCase "before the fallback time: no offer, no alert, the scheduled fallback comes later" $
            afterNoAssignment (run (minutesBefore 90) Nothing) False 1800 @?= NoAssignmentStep {broadcast = False, alertOps = False, recheckAt = Nothing},
          testCase "at the fallback time, not yet offered: offer it to the ordinary drivers, ops wait for the last look" $
            afterNoAssignment (run (minutesBefore 30) Nothing) False 600 @?= NoAssignmentStep {broadcast = True, alertOps = False, recheckAt = Just (minutesBefore 10)},
          testCase "at the last look, already offered and still uncovered: alert ops, no second offer" $
            afterNoAssignment (run (minutesBefore 30) Nothing) True 600 @?= NoAssignmentStep {broadcast = False, alertOps = True, recheckAt = Just (minutesBefore 10)},
          testCase "inside the buffer, not yet offered: offer it and alert ops at once (no time left to wait)" $
            afterNoAssignment (run (minutesBefore 5) Nothing) False 600 @?= NoAssignmentStep {broadcast = True, alertOps = True, recheckAt = Nothing},
          testCase "inside the buffer, already offered: alert ops, no recheck" $
            afterNoAssignment (run (minutesBefore 5) Nothing) True 600 @?= NoAssignmentStep {broadcast = False, alertOps = True, recheckAt = Nothing}
        ],
      testGroup
        "candidates"
        [ testCase "bound-vehicle drivers come before fleet drivers, each once" $
            fst (selectCandidates [] [candidate "f1" FleetDriver True, candidate "b1" BoundVehicle True, candidate "b1" FleetDriver True, candidate "f2" FleetDriver True])
              @?= [candidate "b1" BoundVehicle True, candidate "f1" FleetDriver True, candidate "f2" FleetDriver True],
          testCase "a driver who dropped the run is left out" $
            selectCandidates ["b1"] [candidate "b1" BoundVehicle True, candidate "f1" FleetDriver True]
              @?= ([candidate "f1" FleetDriver True], [("b1", DroppedRun)]),
          testCase "a vehicle that does not serve the run's tier is left out" $
            selectCandidates [] [candidate "b1" BoundVehicle False, candidate "f1" FleetDriver True]
              @?= ([candidate "f1" FleetDriver True], [("b1", WrongServiceTier)])
        ],
      testGroup
        "clash window"
        [ testCase "a second run 20 minutes after the driver's run is refused" $
            rejectedOnTime (classifyDriver (gapTo True) (runCandidate True) () [holdEndingBefore 20 True]) @?= True,
          testCase "a second run 60 minutes after the driver's run is allowed (left to the distance check)" $
            rejectedOnTime (classifyDriver (gapTo True) (runCandidate True) () [holdEndingBefore 60 True]) @?= False,
          testCase "an ordinary ride 20 minutes after an ordinary ride keeps the plain buffer" $
            rejectedOnTime (classifyDriver (gapTo False) (runCandidate False) () [holdEndingBefore 20 False]) @?= False,
          testCase "an ordinary ride 20 minutes after a run keeps the clash window" $
            rejectedOnTime (classifyDriver (gapTo False) (runCandidate False) () [holdEndingBefore 20 True]) @?= True
        ]
    ]
