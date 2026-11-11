{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabCheckInTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "beckn-spec" Domain.Types.SharedCabSession (SessionCheckIn (..))
import "rider-app" SharedLogic.SharedCab.CheckIn
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import "rider-app" Tools.Error (SharedCabSessionError (..))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 11 10) 36000

open :: CheckIn
open = CheckIn {checkInId = "c1", offSince = addUTCTime (-60) t0, deadline = addUTCTime 60 t0, answered = False}

tests :: TestTree
tests =
  testGroup
    "SharedCab check-in"
    [ testCase "the session shows the whole seconds left, rounded down, never below 0" $
        map (\sec -> checkInView (addUTCTime sec t0) open) [0, 0.5, 59.9, 60, 90]
          @?= map (Just . SessionCheckIn "c1") [60, 59, 0, 0, 0],
      testCase "an answered check-in is no longer shown" $
        checkInView t0 open {answered = True} @?= Nothing,
      testCase "an answer naming another check-in, naming none, or with none open is refused" $
        [answerCheckIn (Just "c0") (Just open), answerCheckIn Nothing (Just open), answerCheckIn (Just "c1") Nothing]
          @?= replicate 3 (Left CheckInClosed),
      testCase "the matching answer marks it answered, and answering it again changes nothing" $
        (answerCheckIn (Just "c1") (Just open), answerCheckIn (Just "c1") (Just open {answered = True}))
          @?= (Right open {answered = True}, Right open {answered = True})
    ]
