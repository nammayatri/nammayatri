{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Boundaries of the shuttle subscription rules (SharedLogic.ShuttlePassRules): the 5-hour change cutoff,
-- the ticket issue window, when the boarding OTP goes out, skip-day merging and the OTP compare.
module ShuttlePassRulesTests (tests) where

import qualified Data.Text as T
import qualified Data.Time as DT
import "rider-app" Domain.Types.BoardingOtp (BoardingOtp (..))
import qualified "beckn-spec" Domain.Types.FRFSTicketBookingStatus as DStatus
import qualified "rider-app" Domain.Types.PurchasedPassPayment as DPPP
import "mobility-core" Kernel.Prelude
import "mobility-core" Kernel.Types.Common (Seconds (..))
import qualified "rider-app" SharedLogic.ShuttlePassRules as R
import Test.Tasty
import Test.Tasty.HUnit
import qualified Prelude as P

ist :: Seconds
ist = Seconds 19800

day0, day1, dayPrev :: DT.Day
day0 = DT.fromGregorian 2026 11 25
day1 = DT.addDays 1 day0
dayPrev = DT.addDays (-1) day0

-- | Local IST wall-clock time on a day, as UTC.
localAt :: DT.Day -> Int -> Int -> UTCTime
localAt day h m = R.departureAt ist day (h * 3600 + m * 60)

utc :: Integer -> Int -> Int -> Int -> Int -> UTCTime
utc y mo d h mi = DT.UTCTime (DT.fromGregorian y mo d) (fromIntegral (h * 3600 + mi * 60))

-- The 08:00 slot of day0.
dep :: UTCTime
dep = localAt day0 8 0

tests :: TestTree
tests =
  testGroup
    "Shuttle pass rules"
    [ testGroup
        "time"
        [ testCase "IST midnight of a day is 18:30 UTC the day before" $
            R.localMidnightUtc ist day0 @?= utc 2026 11 24 18 30,
          testCase "08:00 IST departure is 02:30 UTC" $
            dep @?= utc 2026 11 25 2 30,
          testCase "GTFS time past 24h lands on the next calendar day" $
            R.departureAt ist day0 (25 * 3600 + 30 * 60) @?= utc 2026 11 25 20 0,
          testCase "00:30 IST belongs to the new local day" $
            R.localDay ist (utc 2026 11 24 19 0) @?= day0,
          testCase "23:59 IST still belongs to the old local day" $
            R.localDay ist (utc 2026 11 24 18 29) @?= dayPrev
        ],
      testGroup
        "issue window (departure - 5h .. departure)"
        [ testCase "a minute before the cutoff: not yet" $ R.inIssueWindow (localAt day0 2 59) dep @?= False,
          testCase "at the cutoff: issue" $ R.inIssueWindow (localAt day0 3 0) dep @?= True,
          testCase "a minute before departure: issue" $ R.inIssueWindow (localAt day0 7 59) dep @?= True,
          testCase "at departure: too late" $ R.inIssueWindow (localAt day0 8 0) dep @?= False,
          testCase "an early 02:00 slot is issued the evening before" $
            R.inIssueWindow (localAt dayPrev 21 30) (localAt day0 2 0) @?= True
        ],
      testGroup
        "OTP notification (an hour before; waits for a vehicle until 15 minutes before)"
        [ testCase "61 minutes before, vehicle assigned: not yet" $ R.otpDue (localAt day0 6 59) dep True @?= False,
          testCase "60 minutes before, vehicle assigned: send" $ R.otpDue (localAt day0 7 0) dep True @?= True,
          testCase "60 minutes before, no vehicle: wait" $ R.otpDue (localAt day0 7 0) dep False @?= False,
          testCase "16 minutes before, no vehicle: wait" $ R.otpDue (localAt day0 7 44) dep False @?= False,
          testCase "15 minutes before, no vehicle: send anyway" $ R.otpDue (localAt day0 7 45) dep False @?= True,
          testCase "at departure: never" $ R.otpDue (localAt day0 8 0) dep True @?= False
        ],
      testGroup
        "skip / unskip cutoff"
        [ testCase "a past day is closed" $ R.dayOpenForChange (localAt day0 0 10) day0 Nothing dayPrev @?= False,
          testCase "today before the cutoff is open" $ R.dayOpenForChange (localAt day0 2 59) day0 (Just dep) day0 @?= True,
          testCase "today at the cutoff is closed" $ R.dayOpenForChange (localAt day0 3 0) day0 (Just dep) day0 @?= False,
          testCase "tomorrow's 02:00 slot closes this evening" $
            R.dayOpenForChange (localAt day0 21 0) day0 (Just (localAt day1 2 0)) day1 @?= False,
          testCase "unknown departure: today is closed" $ R.dayOpenForChange (localAt day0 0 10) day0 Nothing day0 @?= False,
          testCase "unknown departure: tomorrow is open" $ R.dayOpenForChange (localAt day0 23 0) day0 Nothing day1 @?= True
        ],
      testGroup
        "ticket windows and statuses"
        [ testCase "departure window: 6 hours before to 1 hour after" $
            R.liveTicketWindow dep @?= (localAt day0 2 0, localAt day0 9 0),
          testCase "whole-day window: from 5 hours before midnight to 1 hour after the next" $
            R.dayTicketWindow ist day0 @?= (localAt dayPrev 19 0, localAt day1 1 0),
          testCase "confirming and confirmed count as live" $
            map R.isLiveStatus [DStatus.CONFIRMING, DStatus.CONFIRMED] @?= [True, True],
          testCase "failed, cancelled, counter-cancelled, rescheduled do not" $
            map R.isLiveStatus [DStatus.FAILED, DStatus.CANCELLED, DStatus.COUNTER_CANCELLED, DStatus.RESCHEDULED] @?= [False, False, False, False]
        ],
      testGroup
        "skip days"
        [ testCase "adds new days once, keeps an existing day and its reason" $
            R.addSkipDays [skip day0 (Just "travel")] [day0, day1, day1] (Just "leave")
              @?= [skip day0 (Just "travel"), skip day1 (Just "leave")],
          testCase "adding only existing days changes nothing" $
            R.addSkipDays [skip day0 Nothing] [day0] Nothing @?= [skip day0 Nothing],
          testCase "removes only the given days" $
            R.removeSkipDays [skip day0 Nothing, skip day1 Nothing] [day1, dayPrev] @?= [skip day0 Nothing]
        ],
      testGroup
        "slot serves pickup then drop (GTFS sequence numbers)"
        [ testCase "pickup before drop" $ R.servesInOrder [2] [5] @?= True,
          testCase "drop before pickup" $ R.servesInOrder [5] [2] @?= False,
          testCase "a drop stop occurring twice: B/1, A/2, B/3 serves A -> B" $ R.servesInOrder [2] [1, 3] @?= True,
          testCase "a pickup stop occurring twice: A/1, B/2, A/3 serves A -> B" $ R.servesInOrder [1, 3] [2] @?= True,
          testCase "every drop occurrence before the pickup" $ R.servesInOrder [4] [1, 3] @?= False,
          testCase "same stop is not a trip" $ R.servesInOrder [3] [3] @?= False,
          testCase "stop missing from the trip" $ (R.servesInOrder [] [3], R.servesInOrder [3] []) @?= (False, False)
        ],
      testGroup
        "OTP"
        [ testCase "zero-padded to 4 digits" $ map R.formatOtp [7, 1234, 12345] @?= ["0007", "1234", "2345"],
          testCase "exact code matches" $ R.otpMatches (Just "0420") "0420" @?= True,
          testCase "surrounding spaces are ignored" $ R.otpMatches (Just "0420") " 0420 " @?= True,
          testCase "a padded stored code still matches" $ R.otpMatches (Just "0420 ") "0420" @?= True,
          testCase "wrong code is rejected" $ R.otpMatches (Just "0420") "0421" @?= False,
          testCase "leading zeros matter" $ R.otpMatches (Just "0007") "7" @?= False,
          testCase "no stored code never matches" $ R.otpMatches Nothing "0420" @?= False,
          testCase "an empty code never matches" $ R.otpMatches (Just "") "" @?= False,
          testCase "showing a booking's OTP (as logs do) never prints the code" $
            ("4821" `T.isInfixOf` T.pack (P.show (Just (BoardingOtp "4821")))) @?= False
        ]
    ]
  where
    skip day reason = DPPP.SkipDay {day, reason}
