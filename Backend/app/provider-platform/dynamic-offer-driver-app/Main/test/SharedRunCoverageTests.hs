{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | What keeps a shared-route run covered: the check-in, the fallback's decision, who it tries and when ops hear of it.
module SharedRunCoverageTests (tests) where

import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
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
        [ testCase "before the fallback time: no alert, the scheduled fallback comes later" $
            afterNoAssignment (run (minutesBefore 90) Nothing) 1800 @?= NoAssignmentStep {alertOps = False, recheckAt = Nothing},
          testCase "at the fallback time: alert ops and look again before the buffer closes" $
            afterNoAssignment (run (minutesBefore 30) Nothing) 600 @?= NoAssignmentStep {alertOps = True, recheckAt = Just (minutesBefore 10)},
          testCase "inside the buffer already: alert ops, no recheck" $
            afterNoAssignment (run (minutesBefore 5) Nothing) 600 @?= NoAssignmentStep {alertOps = True, recheckAt = Nothing}
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
        ]
    ]
