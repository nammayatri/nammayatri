{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module TimeWindowBookingTests (tests) where

import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Kernel.Types.Common (Seconds (..))
import "rider-app" SharedLogic.TimeWindowBooking
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

ist :: Seconds
ist = Seconds 19800

t :: String -> UTCTime
t = fromJust . iso8601ParseM

tariff :: A.Value -> DurationTariff
tariff v = case A.fromJSON v of
  A.Success x -> x
  A.Error e -> error e

progressive, timeBased :: DurationTariff
progressive = tariff $ A.object ["tariffType" A..= ("progressive" :: String), "rules" A..= A.object ["base_fare" A..= (20 :: Int), "slabs" A..= [slab 0 10, slab 2 15, slab 5 25]]]
  where
    slab :: Int -> Int -> A.Value
    slab h r = A.object ["min_hours" A..= h, "rate_per_hour" A..= r]
timeBased = tariff $ A.object ["tariffType" A..= ("time_based" :: String), "rules" A..= [band "08:00" "20:00" 20, band "20:00" "08:00" 7]]
  where
    band :: String -> String -> Int -> A.Value
    band s e r = A.object ["start_time" A..= s, "end_time" A..= e, "rate_per_hour" A..= r]

-- Expected fees are ParkWise tariff.ts output for the same inputs, run with TZ=Asia/Kolkata.
tests :: TestTree
tests =
  testGroup
    "TimeWindowBooking"
    [ testCase "flat rounds half up" $ calculateFee ist (FlatTariff 49.995) (t "2026-01-01T04:30:00Z") (t "2026-01-01T06:00:00Z") @?= 50,
      testCase "progressive 45m" $ calculateFee ist progressive (t "2026-01-01T04:30:00Z") (t "2026-01-01T05:15:00Z") @?= 27.5,
      testCase "progressive 4h" $ calculateFee ist progressive (t "2026-01-01T04:30:00Z") (t "2026-01-01T08:30:00Z") @?= 70,
      testCase "progressive 8h20m" $ calculateFee ist progressive (t "2026-01-01T04:30:00Z") (t "2026-01-01T12:50:00Z") @?= 168.33,
      testCase "time based across 20:00 IST" $ calculateFee ist timeBased (t "2026-01-01T13:30:00Z") (t "2026-01-01T16:00:00Z") @?= 30.5,
      testCase "time based overnight" $ calculateFee ist timeBased (t "2026-01-01T13:40:00Z") (t "2026-01-02T03:10:00Z") @?= 114,
      testCase "empty window is free" $ calculateFee ist progressive (t "2026-01-01T05:00:00Z") (t "2026-01-01T05:00:00Z") @?= 0,
      testCase "no start means now" $ resolveBookingStart ist 0 now Nothing @?= Right now,
      testCase "start within 2 min means now" $ resolveBookingStart ist 0 now (Just (t "2026-01-01T05:31:00Z")) @?= Right now,
      testCase "future start is kept as given" $ resolveBookingStart ist 0 now (Just (t "2026-01-01T06:30:00Z")) @?= Right (t "2026-01-01T06:30:00Z"),
      testCase "any minute is allowed" $ resolveBookingStart ist 0 now (Just (t "2026-01-01T06:47:00Z")) @?= Right (t "2026-01-01T06:47:00Z"),
      testCase "start in the past" $ isLeft (resolveBookingStart ist 0 now (Just (t "2026-01-01T04:00:00Z"))) @?= True,
      testCase "tomorrow not allowed with 0 advance days" $ isLeft (resolveBookingStart ist 0 now (Just (t "2026-01-01T18:30:00Z"))) @?= True,
      testCase "tomorrow allowed with 1 advance day" $ resolveBookingStart ist 1 now (Just (t "2026-01-01T18:30:00Z")) @?= Right (t "2026-01-01T18:30:00Z"),
      testCase "window over max" $ isLeft (checkBookingWindow 120 (t "2026-01-01T06:00:00Z") (t "2026-01-01T08:30:00Z")) @?= True,
      testCase "window at max" $ checkBookingWindow 120 (t "2026-01-01T06:00:00Z") (t "2026-01-01T08:00:00Z") @?= Right (),
      testCase "peak counts concurrent windows only" $
        peakOverlap (t "2026-01-01T06:00:00Z") (t "2026-01-01T09:00:00Z") [w "05:00" "06:30", w "06:30" "07:00", w "07:00" "08:00", w "08:30" "10:00", w "09:00" "10:00"] @?= 1,
      testCase "peak with nested windows" $
        peakOverlap (t "2026-01-01T06:00:00Z") (t "2026-01-01T09:00:00Z") [w "05:00" "08:00", w "06:30" "07:30", w "07:00" "07:10"] @?= 3
    ]
  where
    now = t "2026-01-01T05:30:00Z" -- 11:00 IST
    w a b = (t ("2026-01-01T" <> a <> ":00Z"), t ("2026-01-01T" <> b <> ":00Z"))
    isLeft = either (const True) (const False)
