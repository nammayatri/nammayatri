{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabExpiryTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "rider-app" SharedLogic.SharedCab.SessionState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 25) 0

minutes :: Int -> UTCTime
minutes n = addUTCTime (fromIntegral (n * 60)) t0

-- 15 min pause, 60 min end, last ping at t0.
decide :: Int -> SessionStatus -> Maybe ExpiryAction
decide nowMin = expiryAction (15 * 60) (60 * 60) (minutes nowMin) t0

tests :: TestTree
tests =
  testGroup
    "SharedCab expiry"
    [ testCase "a fresh ping keeps the session" $
        decide 14 ACTIVE @?= Nothing,
      testCase "stale ping past pauseAfter pauses an ACTIVE session" $
        decide 15 ACTIVE @?= Just PauseSilent,
      testCase "an already PAUSED session isn't paused again" $
        decide 30 PAUSED @?= Nothing,
      testCase "stale ping past endAfter ends a PAUSED session" $
        decide 60 PAUSED @?= Just EndSilent,
      testCase "stale ping past endAfter ends an ACTIVE one too (missed ticks)" $
        decide 90 ACTIVE @?= Just EndSilent,
      testCase "an ENDED session is left alone" $
        decide 90 ENDED @?= Nothing,
      testCase "a ping more than 5 min ahead (ms epoch, skewed clock) or missing is unreadable" $
        map (readPing (minutes 10)) [Just (minutes 9), Just (minutes 15), Just (minutes 16), Nothing]
          @?= [SeenAt (minutes 9), SeenAt (minutes 15), Unreadable, Unreadable],
      testCase "an unreadable ping is skipped only until the trip is endAfter old" $
        map (\(nowMin, ping) -> lastSeenFor (60 * 60) (minutes nowMin) t0 ping) [(30, Just Unreadable), (60, Just Unreadable), (30, Nothing), (30, Just (SeenAt (minutes 20)))]
          @?= [Nothing, Just t0, Just t0, Just (minutes 20)]
    ]
