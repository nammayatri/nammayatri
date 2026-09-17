module OfferFrequencyTests (offerFrequencyTests) where

import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Common (Seconds (..))
import Lib.Payment.Domain.Types.Offer (OfferFrequency (..))
import Lib.Payment.Offer.Frequency
import Test.Tasty
import Test.Tasty.HUnit

offerFrequencyTests :: TestTree
offerFrequencyTests =
  testGroup
    "Offer usage counter windows (Lib.Payment.Offer.Frequency)"
    [ hourlyTests,
      dailyTests,
      weeklyTests,
      monthlyTests,
      offsetTests,
      containmentProperty
    ]

ist :: Seconds
ist = Seconds 19800

utc :: Integer -> Int -> Int -> Integer -> Integer -> Time.UTCTime
utc y m d h mi = Time.UTCTime (Time.fromGregorian y m d) (Time.secondsToDiffTime (h * 3600 + mi * 60))

istTime :: Integer -> Int -> Int -> Integer -> Integer -> Time.UTCTime
istTime y m d h mi = Time.addUTCTime (negate 19800) (utc y m d h mi)

istWindow :: (Integer, Int, Int, Integer, Integer) -> (Integer, Int, Int, Integer, Integer) -> Window
istWindow (y1, m1, d1, h1, mi1) (y2, m2, d2, h2, mi2) = Window (istTime y1 m1 d1 h1 mi1) (istTime y2 m2 d2 h2 mi2)

hourlyTests :: TestTree
hourlyTests =
  testGroup
    "HOURLY is the local clock hour"
    [ testCase "15:20 IST is in 15:00 .. 16:00 IST" $
        windowOf HOURLY ist (istTime 2026 9 8 15 20) @?= istWindow (2026, 9, 8, 15, 0) (2026, 9, 8, 16, 0),
      testCase "15:59 IST is still the same hour, 16:00 IST starts the next" $ do
        windowOf HOURLY ist (istTime 2026 9 8 15 59) @?= istWindow (2026, 9, 8, 15, 0) (2026, 9, 8, 16, 0)
        windowOf HOURLY ist (istTime 2026 9 8 16 0) @?= istWindow (2026, 9, 8, 16, 0) (2026, 9, 8, 17, 0),
      testCase "the last hour of the local day ends at local midnight" $
        windowOf HOURLY ist (istTime 2026 9 8 23 40) @?= istWindow (2026, 9, 8, 23, 0) (2026, 9, 9, 0, 0),
      testCase "a clock hour is exactly 3600 seconds" $
        let w = windowOf HOURLY ist (istTime 2026 9 8 15 20) in Time.diffUTCTime w.windowEnd w.windowStart @?= 3600
    ]

dailyTests :: TestTree
dailyTests =
  testGroup
    "DAILY is the local calendar day"
    [ testCase "02:00 IST on 8 Sep (20:30 UTC on 7 Sep) belongs to 8 Sep" $
        windowOf DAILY ist (utc 2026 9 7 20 30) @?= istWindow (2026, 9, 8, 0, 0) (2026, 9, 9, 0, 0),
      testCase "23:59 IST on 7 Sep still belongs to 7 Sep" $
        windowOf DAILY ist (istTime 2026 9 7 23 59) @?= istWindow (2026, 9, 7, 0, 0) (2026, 9, 8, 0, 0),
      testCase "the window starts at 18:30 UTC of the previous day" $
        (windowOf DAILY ist (istTime 2026 9 8 12 0)).windowStart @?= utc 2026 9 7 18 30
    ]

weeklyTests :: TestTree
weeklyTests =
  testGroup
    "WEEKLY is Monday 00:00 .. Sunday 23:59 local (ISO week)"
    [ testCase "Tue 8 Sep 2026 is in Mon 7 Sep .. Mon 14 Sep" $
        windowOf WEEKLY ist (istTime 2026 9 8 15 20) @?= istWindow (2026, 9, 7, 0, 0) (2026, 9, 14, 0, 0),
      testCase "Monday 00:00 opens the week, Sunday 23:59 closes it" $ do
        windowOf WEEKLY ist (istTime 2026 9 7 0 0) @?= istWindow (2026, 9, 7, 0, 0) (2026, 9, 14, 0, 0)
        windowOf WEEKLY ist (istTime 2026 9 13 23 59) @?= istWindow (2026, 9, 7, 0, 0) (2026, 9, 14, 0, 0)
        windowOf WEEKLY ist (istTime 2026 9 14 0 0) @?= istWindow (2026, 9, 14, 0, 0) (2026, 9, 21, 0, 0),
      testCase "a week crossing a month boundary: Wed 30 Sep is in Mon 28 Sep .. Mon 5 Oct" $
        windowOf WEEKLY ist (istTime 2026 9 30 9 0) @?= istWindow (2026, 9, 28, 0, 0) (2026, 10, 5, 0, 0),
      testCase "a week crossing a year boundary: Thu 31 Dec 2026 is in Mon 28 Dec .. Mon 4 Jan 2027" $
        windowOf WEEKLY ist (istTime 2026 12 31 9 0) @?= istWindow (2026, 12, 28, 0, 0) (2027, 1, 4, 0, 0),
      testCase "a week is exactly seven days" $
        let w = windowOf WEEKLY ist (istTime 2026 9 8 15 20) in Time.diffUTCTime w.windowEnd w.windowStart @?= 7 * 86400
    ]

monthlyTests :: TestTree
monthlyTests =
  testGroup
    "MONTHLY is the local calendar month"
    [ testCase "8 Sep 2026 is in 1 Sep .. 1 Oct" $
        windowOf MONTHLY ist (istTime 2026 9 8 15 20) @?= istWindow (2026, 9, 1, 0, 0) (2026, 10, 1, 0, 0),
      testCase "30 Sep 23:59 IST is September, 1 Oct 00:00 IST is October" $ do
        windowOf MONTHLY ist (istTime 2026 9 30 23 59) @?= istWindow (2026, 9, 1, 0, 0) (2026, 10, 1, 0, 0)
        windowOf MONTHLY ist (istTime 2026 10 1 0 0) @?= istWindow (2026, 10, 1, 0, 0) (2026, 11, 1, 0, 0),
      testCase "February 2028 has 29 days (leap year)" $ do
        let w = windowOf MONTHLY ist (istTime 2028 2 10 12 0)
        w @?= istWindow (2028, 2, 1, 0, 0) (2028, 3, 1, 0, 0)
        Time.diffUTCTime w.windowEnd w.windowStart @?= 29 * 86400,
      testCase "February 2027 has 28 days" $
        let w = windowOf MONTHLY ist (istTime 2027 2 10 12 0) in Time.diffUTCTime w.windowEnd w.windowStart @?= 28 * 86400,
      testCase "31-day and 30-day months" $ do
        let jan = windowOf MONTHLY ist (istTime 2026 1 15 12 0)
            sep = windowOf MONTHLY ist (istTime 2026 9 15 12 0)
        Time.diffUTCTime jan.windowEnd jan.windowStart @?= 31 * 86400
        Time.diffUTCTime sep.windowEnd sep.windowStart @?= 30 * 86400,
      testCase "December rolls into the next year" $
        windowOf MONTHLY ist (istTime 2026 12 31 23 0) @?= istWindow (2026, 12, 1, 0, 0) (2027, 1, 1, 0, 0)
    ]

offsetTests :: TestTree
offsetTests =
  testGroup
    "the city's UTC offset decides the local day"
    [ testCase "zero offset: 20:30 UTC on 7 Sep is 7 Sep" $
        windowOf DAILY (Seconds 0) (utc 2026 9 7 20 30) @?= Window (utc 2026 9 7 0 0) (utc 2026 9 8 0 0),
      testCase "negative offset (UTC-5): 03:00 UTC on 8 Sep is still 7 Sep locally" $
        (windowOf DAILY (Seconds (-18000)) (utc 2026 9 8 3 0)).windowStart @?= utc 2026 9 7 5 0,
      testCase "two cities disagree on the day at the same instant" $
        assertBool "IST and UTC-5 differ" $
          (windowOf DAILY ist (utc 2026 9 7 20 30)).windowStart /= (windowOf DAILY (Seconds (-18000)) (utc 2026 9 7 20 30)).windowStart
    ]

containmentProperty :: TestTree
containmentProperty =
  testCase "windows tile time without gaps or overlaps" $
    forM_ allOfferFrequencies $ \frequency ->
      forM_ [0 .. 1370 :: Integer] $ \n -> do
        let t = Time.addUTCTime (fromInteger (n * 7 * 3600)) (utc 2027 12 1 0 0)
            w = windowOf frequency ist t
            label = show frequency <> " at " <> show t
        assertBool (label <> ": contains t") (w.windowStart <= t && t < w.windowEnd)
        assertBool (label <> ": start is stable") ((windowOf frequency ist w.windowStart).windowStart == w.windowStart)
        assertBool (label <> ": last second is inside") ((windowOf frequency ist (Time.addUTCTime (-1) w.windowEnd)).windowStart == w.windowStart)
        assertBool (label <> ": end opens the next window") ((windowOf frequency ist w.windowEnd).windowStart == w.windowEnd)
