module SlidingWindowLimiter where

import Beckn.Utils.SlidingWindowLimiter (slidingWindowLimiterPure)
import Data.Time
import Data.Time.Clock.POSIX
import EulerHS.Prelude
import Test.Tasty
import Test.Tasty.HUnit

hitsLimit :: Int
hitsLimit = 4

frameLen :: Int
frameLen = 10

frame0 :: UTCTime
frame0 = posixSecondsToUTCTime $ secondsToNominalDiffTime 0

frame1 :: UTCTime
frame1 = posixSecondsToUTCTime $ secondsToNominalDiffTime (fromIntegral frameLen + 1)

frame1Late :: UTCTime
frame1Late = posixSecondsToUTCTime $ secondsToNominalDiffTime (fromIntegral frameLen * 2 - 1)

frame2 :: UTCTime
frame2 = posixSecondsToUTCTime $ secondsToNominalDiffTime (fromIntegral frameLen * 2 + 1)

emptyList :: TestTree
emptyList = testCase "Empty list" $ do
  slidingWindowLimiterPure frame0 [] hitsLimit frameLen @?= ([0], True)
  slidingWindowLimiterPure frame1 [] hitsLimit frameLen @?= ([1], True)
  slidingWindowLimiterPure frame2 [] hitsLimit frameLen @?= ([2], True)

successful :: TestTree
successful = testCase "Successful tests" $ do
  slidingWindowLimiterPure frame0 [0, 0] hitsLimit frameLen @?= ([0, 0, 0], True)
  slidingWindowLimiterPure frame1 [0, 0, 0] hitsLimit frameLen @?= ([1, 0, 0, 0], True)
  slidingWindowLimiterPure frame1 [0, 0, 0, 0] hitsLimit frameLen @?= ([1, 0, 0, 0, 0], True)
  slidingWindowLimiterPure frame1Late [0, 0, 0, 0, 1] hitsLimit frameLen @?= ([1, 0, 0, 0, 0, 1], True)
  slidingWindowLimiterPure frame2 [0, 0, 1, 1, 0, 0, 1] hitsLimit frameLen @?= ([2, 1, 1, 1], True)

failing :: TestTree
failing = testCase "Failing tests" $ do
  slidingWindowLimiterPure frame0 [0, 0, 0, 0] hitsLimit frameLen @?= ([0, 0, 0, 0], False)
  slidingWindowLimiterPure frame1 [0, 0, 0, 0, 1] hitsLimit frameLen @?= ([0, 0, 0, 0, 1], False)
  slidingWindowLimiterPure frame1 [0, 0, 0, 0, 1, 1, 1, 1] hitsLimit frameLen @?= ([0, 0, 0, 0, 1, 1, 1, 1], False)
  slidingWindowLimiterPure frame2 [0, 0, 1, 1, 0, 2, 1, 1] hitsLimit frameLen @?= ([1, 1, 2, 1, 1], False)

slidingWindowLimiterTests :: TestTree
slidingWindowLimiterTests =
  testGroup
    "Sliding window limiter tests"
    [ emptyList,
      successful,
      failing
    ]
