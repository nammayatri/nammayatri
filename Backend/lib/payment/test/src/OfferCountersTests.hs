module OfferCountersTests (offerCountersTests) where

import qualified Data.Time as Time
import Kernel.Prelude
import Kernel.Types.Common (Currency (..), HighPrecMoney, Seconds (..))
import Kernel.Types.Id (Id (..))
import Lib.Payment.Domain.Types.Offer (OfferFrequency (..))
import qualified Lib.Payment.Domain.Types.OfferStats as DOfferStats
import Lib.Payment.Domain.Types.PersonOfferFrequencyStats (PersonOfferFrequencyStats (..))
import Lib.Payment.Offer.Counters
import Lib.Payment.Offer.Frequency (Window (..), allOfferFrequencies, windowOf)
import Test.Tasty
import Test.Tasty.HUnit

offerCountersTests :: TestTree
offerCountersTests =
  testGroup
    "Offer usage counter (Lib.Payment.Offer.Counters)"
    [ isCurrentTests,
      bumpAndCurrentCountTests,
      applyToRowTests,
      countFromRowTests,
      frequencySwitchTests,
      writeReadAgreement,
      isUsedUpTests
    ]

ist :: Seconds
ist = Seconds 19800

istTime :: Integer -> Int -> Int -> Integer -> Integer -> Time.UTCTime
istTime y m d h mi = Time.addUTCTime (negate 19800) (Time.UTCTime (Time.fromGregorian y m d) (Time.secondsToDiffTime (h * 3600 + mi * 60)))

windowAt :: OfferFrequency -> Time.UTCTime -> Window
windowAt frequency = windowOf frequency ist

rowWrittenAt :: OfferFrequency -> Time.UTCTime -> Int -> PersonOfferFrequencyStats
rowWrittenAt frequency t n =
  PersonOfferFrequencyStats
    { id = Id "row",
      entityId = "rider",
      entityType = DOfferStats.Person,
      offerId = Id "offer",
      appliedCount = n,
      periodStart = Just (windowAt frequency t).windowStart,
      totalDiscountAmount = 10,
      totalCashbackAmount = 20,
      currency = INR,
      merchantId = "merchant",
      merchantOperatingCityId = "city",
      createdAt = t,
      updatedAt = t
    }

countAfter :: OfferFrequency -> Time.UTCTime -> Time.UTCTime -> Int
countAfter frequency earlier later = (applyToRow (windowAt frequency later) later 0 0 (rowWrittenAt frequency earlier 1)).appliedCount

t0 :: Time.UTCTime
t0 = istTime 2026 9 8 15 20

hourW :: Window
hourW = windowAt HOURLY t0

previousHour, nextHour :: Time.UTCTime
previousHour = Time.addUTCTime (-3600) hourW.windowStart
nextHour = Time.addUTCTime 3600 hourW.windowStart

isCurrentTests :: TestTree
isCurrentTests =
  testGroup
    "isCurrent"
    [ testCase "the same period start: current" $ isCurrent hourW (Just hourW.windowStart) @?= True,
      testCase "an older period start: stale" $ isCurrent hourW (Just previousHour) @?= False,
      testCase "no period stored: stale" $ isCurrent hourW Nothing @?= False,
      testCase "a later period start (written by a ride that crossed the boundary first): current, never moved back" $
        isCurrent hourW (Just nextHour) @?= True
    ]

bumpAndCurrentCountTests :: TestTree
bumpAndCurrentCountTests =
  testGroup
    "bumpCount (write side) and currentCount (read side)"
    [ testCase "same period: one more / the stored count" $ do
        bumpCount hourW 3 (Just hourW.windowStart) @?= 4
        currentCount hourW 3 (Just hourW.windowStart) @?= 3,
      testCase "an older period: this apply is the first / zero" $ do
        bumpCount hourW 3 (Just previousHour) @?= 1
        currentCount hourW 3 (Just previousHour) @?= 0,
      testCase "nothing stored yet: first / zero" $ do
        bumpCount hourW 3 Nothing @?= 1
        currentCount hourW 3 Nothing @?= 0,
      testCase "a later period: counted into it / visible" $ do
        bumpCount hourW 3 (Just nextHour) @?= 4
        currentCount hourW 3 (Just nextHour) @?= 3
    ]

applyToRowTests :: TestTree
applyToRowTests =
  testGroup
    "applyToRow rolls the offer's period"
    [ testCase "HOURLY, second apply in the same hour: 2, money adds up" $ do
        let row = applyToRow hourW t0 5 0 (rowWrittenAt HOURLY t0 1)
        row.appliedCount @?= 2
        row.periodStart @?= Just hourW.windowStart
        row.totalDiscountAmount @?= (15 :: HighPrecMoney)
        row.totalCashbackAmount @?= (20 :: HighPrecMoney),
      testCase "HOURLY, 15:59 then 16:01: back to 1, the period moves, money starts again" $ do
        let later = istTime 2026 9 8 16 1
            row = applyToRow (windowAt HOURLY later) later 0 7 (rowWrittenAt HOURLY (istTime 2026 9 8 15 59) 3)
        row.appliedCount @?= 1
        row.periodStart @?= Just (windowAt HOURLY later).windowStart
        row.totalDiscountAmount @?= (0 :: HighPrecMoney)
        row.totalCashbackAmount @?= (7 :: HighPrecMoney),
      testCase "DAILY, 15:20 then 23:59 the same day: 2" $ countAfter DAILY t0 (istTime 2026 9 8 23 59) @?= 2,
      testCase "DAILY, 23:59 Tue then 00:01 Wed: 1" $ countAfter DAILY (istTime 2026 9 8 23 59) (istTime 2026 9 9 0 1) @?= 1,
      testCase "WEEKLY, Tue 8 Sep then Sun 13 Sep: 2" $ countAfter WEEKLY t0 (istTime 2026 9 13 23 59) @?= 2,
      testCase "WEEKLY, Sun 13 Sep then Mon 14 Sep: 1" $ countAfter WEEKLY (istTime 2026 9 13 23 59) (istTime 2026 9 14 0 1) @?= 1,
      testCase "MONTHLY, 8 Sep then 30 Sep: 2" $ countAfter MONTHLY t0 (istTime 2026 9 30 23 59) @?= 2,
      testCase "MONTHLY, 30 Sep then 1 Oct: 1" $ countAfter MONTHLY (istTime 2026 9 30 23 59) (istTime 2026 10 1 0 1) @?= 1,
      testCase "a ride that ended at 15:59 but is written after one from 16:00: counted into 16:00, the period stays" $ do
        let row = applyToRow (windowAt HOURLY (istTime 2026 9 8 15 59)) t0 0 0 (rowWrittenAt HOURLY (istTime 2026 9 8 16 0) 1)
        row.appliedCount @?= 2
        row.periodStart @?= Just (windowAt HOURLY (istTime 2026 9 8 16 0)).windowStart
    ]

countFromRowTests :: TestTree
countFromRowTests =
  testGroup
    "countFromRow shows a stored count only while its period is current"
    [ testCase "no row: zero" $ countFromRow hourW Nothing @?= 0,
      testCase "row from this hour: the count" $ countFromRow hourW (Just (rowWrittenAt HOURLY t0 3)) @?= 3,
      testCase "HOURLY row from 15:20 read at 16:05: zero" $
        countFromRow (windowAt HOURLY (istTime 2026 9 8 16 5)) (Just (rowWrittenAt HOURLY t0 3)) @?= 0,
      testCase "DAILY row from Tue read Wed morning: zero" $
        countFromRow (windowAt DAILY (istTime 2026 9 9 10 0)) (Just (rowWrittenAt DAILY t0 3)) @?= 0,
      testCase "WEEKLY row from Tue read Wed morning: the count" $
        countFromRow (windowAt WEEKLY (istTime 2026 9 9 10 0)) (Just (rowWrittenAt WEEKLY t0 3)) @?= 3,
      testCase "MONTHLY row from September read in October: zero" $
        countFromRow (windowAt MONTHLY (istTime 2026 10 5 10 0)) (Just (rowWrittenAt MONTHLY t0 3)) @?= 0,
      testCase "row already in the next hour, read by a clock still in this one: the count" $
        countFromRow hourW (Just (rowWrittenAt HOURLY (istTime 2026 9 8 16 0) 3)) @?= 3
    ]

frequencySwitchTests :: TestTree
frequencySwitchTests =
  testGroup
    "changing the offer's frequency"
    [ testCase "DAILY row from Tue, offer switched to WEEKLY: Tue's applies are inside this week, so they carry over" $ do
        let row = rowWrittenAt DAILY t0 3
            weekly = windowAt WEEKLY (istTime 2026 9 8 18 0)
        countFromRow weekly (Just row) @?= 3
        (applyToRow weekly (istTime 2026 9 8 18 0) 0 0 row).appliedCount @?= 4,
      testCase "WEEKLY row written Tue (period from Mon), offer switched to DAILY: reads zero and the next apply starts at 1" $ do
        let row = rowWrittenAt WEEKLY t0 3
            daily = windowAt DAILY (istTime 2026 9 8 18 0)
        countFromRow daily (Just row) @?= 0
        (applyToRow daily (istTime 2026 9 8 18 0) 0 0 row).appliedCount @?= 1,
      testCase "MONTHLY row from 8 Sep, offer switched to WEEKLY: 1 Sep is before this week's Monday, so it starts again" $ do
        let row = rowWrittenAt MONTHLY t0 3
            weekly = windowAt WEEKLY (istTime 2026 9 8 18 0)
        countFromRow weekly (Just row) @?= 0
        (applyToRow weekly (istTime 2026 9 8 18 0) 0 0 row).appliedCount @?= 1,
      testCase "DAILY row from a Monday, offer switched to WEEKLY: the same start, exact" $ do
        let row = rowWrittenAt DAILY (istTime 2026 9 14 10 0) 3
            weekly = windowAt WEEKLY (istTime 2026 9 16 10 0)
        countFromRow weekly (Just row) @?= 3
        (applyToRow weekly (istTime 2026 9 16 10 0) 0 0 row).appliedCount @?= 4
    ]

writeReadAgreement :: TestTree
writeReadAgreement =
  testCase "an apply is visible immediately after it is written" $
    forM_ allOfferFrequencies $ \frequency ->
      forM_ [0 .. 1370 :: Integer] $ \n -> do
        let t = Time.addUTCTime (fromInteger (n * 7 * 3600)) (istTime 2027 12 1 0 0)
            earlier = Time.addUTCTime (-90000) t
            window = windowAt frequency t
            fresh = applyToRow window t 0 0 (rowWrittenAt frequency earlier 2)
            expected = if isCurrent window (Just (windowAt frequency earlier).windowStart) then 3 else 1
        assertEqual (show frequency <> " at " <> show t) expected (countFromRow window (Just fresh))
        assertEqual (show frequency <> " period start at " <> show t) (Just window.windowStart) fresh.periodStart

isUsedUpTests :: TestTree
isUsedUpTests =
  testGroup
    "isUsedUp (the cap check)"
    [ testCase "no cap: never used up" $ isUsedUp Nothing 100 @?= False,
      testCase "below the cap: still offered, so the last allowed use is paid" $ isUsedUp (Just 5) 4 @?= False,
      testCase "at the cap: used up" $ isUsedUp (Just 5) 5 @?= True,
      testCase "over the cap after it was lowered: used up" $ isUsedUp (Just 3) 4 @?= True,
      testCase "cap raised above the count: offered again" $ isUsedUp (Just 8) 5 @?= False
    ]
