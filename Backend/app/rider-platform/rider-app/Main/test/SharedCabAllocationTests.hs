{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabAllocationTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "rider-app" SharedLogic.SharedCab.Allocation.Types
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 36000

standing :: AllocationState
standing = AllocationState {vehicleNumber = "ML05A1234", allocatedAt = t0, expiresAt = Just (addUTCTime 180 t0), attempts = 0, timerKind = StandTimer}

tests :: TestTree
tests =
  testGroup
    "SharedCab allocation timers and LTS timestamps"
    [ testCase "a running timer keeps the allocation" $
        timerExpiry (addUTCTime 179 t0) (Just standing) @?= Nothing,
      testCase "an expired stand timer releases as StandTimeout, a moving one as MovingTimeout" $
        (timerExpiry (addUTCTime 181 t0) (Just standing), timerExpiry (addUTCTime 181 t0) (Just standing {timerKind = MovingTimer}))
          @?= (Just StandTimeout, Just MovingTimeout),
      testCase "a cab claimed while moving has no timer to lapse" $
        timerExpiry (addUTCTime 3600 t0) (Just standing {expiresAt = Nothing}) @?= Nothing,
      testCase "timer mode follows the cab: moving above 1 m/s, no speed reads as stationary" $
        map isMovingSpeed [Just 5.0, Just 0.2, Nothing] @?= [True, False, False],
      testCase "a vanished alloc key releases without blame" $
        (timerExpiry t0 Nothing, blameFor TimerLost, countsTowardAttempts TimerLost) @?= (Just TimerLost, BlameNone, False),
      testCase "LTS chrono RFC 3339 with nanoseconds and Z" $
        parseLtsTimestamp "2026-09-25T10:00:00.123456789Z" @?= Just (addUTCTime 0.123456789 t0),
      testCase "offsets, a space separator and epoch seconds also parse" $
        map parseLtsTimestamp ["2026-09-25T15:30:00+05:30", "2026-09-25 10:00:00Z", "1790330400"]
          @?= [Just t0, Just t0, Just t0],
      testCase "garbage is not a timestamp" $
        parseLtsTimestamp "yesterday" @?= Nothing
    ]
