{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabRefundPolicyTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import "beckn-spec" Domain.Types.FRFSTicketStatus (FRFSTicketStatus (ACTIVE, INPROGRESS, USED))
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as TS
import "mobility-core" Kernel.External.Maps.Types (LatLong (..))
import "rider-app" SharedLogic.SharedCab.Allocation.Types (RiderFix (..))
import "rider-app" SharedLogic.SharedCab.DriverAction (SharedCabDriverActionError (..), requireReason)
import "rider-app" SharedLogic.SharedCab.LegState (SharedCabState (..))
import "rider-app" SharedLogic.SharedCab.RefundDecision (Refund (..), cancelRefund, refundAmounts, refundWithheld)
import "rider-app" SharedLogic.SharedCab.RefundPolicy
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 30) 36000

stop :: LatLong
stop = LatLong 25.5788 91.8933

-- roughly 55 m and 1.1 km north of the stop
nearby, far :: LatLong
nearby = LatLong 25.5793 91.8933
far = LatLong 25.5888 91.8933

waiting :: [FRFSTicketStatus]
waiting = [ACTIVE]

rider :: SharedCabState -> Int -> Bool -> CancelDecision
rider state noShows near = decideCancel ByRider state noShows waiting near

driver :: SharedCabState -> Int -> Bool -> CancelDecision
driver state noShows near = decideCancel ByDriver state noShows waiting near

-- radius 150 m, fixes fresh for 60 s
nearStop :: Maybe LatLong -> Maybe RiderFix -> Bool
nearStop = riderNearStop 150 60 t0

fixAt :: Int -> LatLong -> Maybe RiderFix
fixAt ageSec p = Just RiderFix {position = p, takenAt = addUTCTime (negate (fromIntegral ageSec)) t0}

tests :: TestTree
tests =
  testGroup
    "shared-cab refund policy (R54)"
    [ testGroup
        "1: no cab allocated"
        [ testCase "FINDING refunds in full" $ rider FINDING 0 True @?= Allowed FullRefund,
          testCase "FALLBACK refunds in full" $ rider FALLBACK 0 True @?= Allowed FullRefund
        ],
      testGroup
        "2: allocated, cab still en route"
        [ testCase "ALLOCATED refunds in full, even beside the stop" $ rider ALLOCATED 0 True @?= Allowed FullRefund
        ],
      testGroup
        "3: cab waiting at the stop"
        [ testCase "rider near: self-cancel refused, talk to the driver" $ rider ARRIVING 0 True @?= Rejected TalkToDriver,
          testCase "rider away: refunds in full" $ rider ARRIVING 0 False @?= Allowed FullRefund,
          testCase "the driver's cancel refunds in full while the rider is near" $ driver ARRIVING 0 True @?= Allowed FullRefund,
          testCase "the driver's cancel refunds in full with the rider away" $ driver ARRIVING 0 False @?= Allowed FullRefund
        ],
      testGroup
        "3: near = within the board radius by a fresh fix; unknown or stale counts as near"
        [ testCase "no fix" $ nearStop (Just stop) Nothing @?= True,
          testCase "no stop to measure from" $ nearStop Nothing (fixAt 5 far) @?= True,
          testCase "stale fix, far" $ nearStop (Just stop) (fixAt 61 far) @?= True,
          testCase "fresh fix at the age limit, far" $ nearStop (Just stop) (fixAt 60 far) @?= False,
          testCase "fresh fix inside the radius" $ nearStop (Just stop) (fixAt 5 nearby) @?= True,
          testCase "fresh fix outside the radius" $ nearStop (Just stop) (fixAt 5 far) @?= False,
          testCase "radius is the tunable, not a constant" $ riderNearStop 20 60 t0 (Just stop) (fixAt 5 nearby) @?= False
        ],
      testGroup
        "4: after a no-show"
        [ testCase "FINDING (reallocated): no refund" $ rider FINDING 1 True @?= Allowed NoRefund,
          testCase "FALLBACK: no refund" $ rider FALLBACK 2 True @?= Allowed NoRefund,
          testCase "ALLOCATED: no refund" $ rider ALLOCATED 1 True @?= Allowed NoRefund,
          testCase "ARRIVING, rider away: no refund" $ rider ARRIVING 1 False @?= Allowed NoRefund,
          testCase "ARRIVING, rider near: still talk to the driver" $ rider ARRIVING 1 True @?= Rejected TalkToDriver,
          testCase "the driver's cancel still refunds in full" $ driver ALLOCATED 1 False @?= Allowed FullRefund
        ],
      testGroup
        "5: boarded"
        [ testCase "BOARDED: refused" $ rider BOARDED 0 False @?= Rejected RideStarted,
          testCase "DEGRADED: refused" $ rider DEGRADED 0 False @?= Rejected RideStarted,
          testCase "DROPPED: refused" $ rider DROPPED 0 False @?= Rejected RideStarted,
          testCase "a ticket INPROGRESS refuses whatever the state says" $ decideCancel ByRider ALLOCATED 0 [INPROGRESS] False @?= Rejected RideStarted,
          testCase "one INPROGRESS among several tickets" $ decideCancel ByRider ARRIVING 0 [ACTIVE, INPROGRESS] False @?= Rejected RideStarted,
          testCase "a USED ticket refuses" $ decideCancel ByRider FINDING 0 [USED] False @?= Rejected RideStarted,
          testCase "boarded after a no-show is still refused, not NoRefund" $ decideCancel ByRider BOARDED 1 [INPROGRESS] False @?= Rejected RideStarted,
          testCase "the driver's cancel does not reach a boarded seat" $ decideCancel ByDriver BOARDED 0 [INPROGRESS] False @?= Rejected RideStarted
        ],
      testGroup
        "6: 'I got down'"
        [ testCase "a boarded seat is dropped" $ routeRiderDrop [INPROGRESS] @?= MarkDropped,
          testCase "boarded wins over a still-waiting ticket" $ routeRiderDrop [ACTIVE, INPROGRESS] @?= MarkDropped,
          testCase "never boarded: a cancel" $ routeRiderDrop [ACTIVE] @?= CancelInstead,
          testCase "several unboarded tickets: a cancel" $ routeRiderDrop [ACTIVE, ACTIVE] @?= CancelInstead,
          testCase "already used or cancelled: nothing to do" $ routeRiderDrop [USED, TS.CANCELLED] @?= NothingToDrop,
          testCase "no tickets" $ routeRiderDrop [] @?= NothingToDrop
        ],
      testGroup
        "state from the booking's plate and the arrival deadline"
        [ testCase "no plate" $ cancelState t0 Nothing Nothing @?= FINDING,
          testCase "plate, no timer" $ cancelState t0 (Just "ML05A9999") Nothing @?= ALLOCATED,
          testCase "plate, timer armed" $ cancelState t0 (Just "ML05A9999") (Just t0) @?= ARRIVING
        ],
      testGroup
        "the refund handed to ExternalBPP"
        [ testCase "full: no charge, whole fare back" $ refundAmounts 40 FullRefund @?= (0, 40),
          testCase "none: whole fare charged, nothing back" $ refundAmounts 40 NoRefund @?= (40, 0),
          testCase "none withholds the payment's refund" $ uncurry refundWithheld (refundAmounts 40 NoRefund) @?= True,
          testCase "full does not" $ uncurry refundWithheld (refundAmounts 40 FullRefund) @?= False,
          testCase "a free booking has nothing to withhold" $ refundWithheld 0 0 @?= False,
          testCase "a partial refund still refunds" $ refundWithheld 10 30 @?= False
        ],
      testGroup
        "every cancel path resolves its refund: none reaches the tier table for shared cab"
        [ testCase "not shared cab: no override, whatever else is set (bus/metro keep their tiers)" $
            [cancelRefund False rider' d | rider' <- [True, False], d <- [Nothing, Just FullRefund, Just NoRefund]] @?= replicate 6 (Right Nothing),
          testCase "shared cab never resolves to the tier table, for any initiator or decision" $
            [cancelRefund True rider' d == Right Nothing | rider' <- [True, False], d <- [Nothing, Just FullRefund, Just NoRefund]] @?= replicate 6 False,
          testCase "shared cab, rider cancel that skipped the policy guard: refused" $ cancelRefund True True Nothing @?= Left (),
          testCase "shared cab, system cancel with no decision: full refund" $ cancelRefund True False Nothing @?= Right (Just FullRefund),
          testCase "shared cab, the guard's decision wins (no refund)" $ cancelRefund True True (Just NoRefund) @?= Right (Just NoRefund),
          testCase "shared cab, the guard's decision wins (full refund)" $ cancelRefund True True (Just FullRefund) @?= Right (Just FullRefund),
          testCase "shared cab, a system cancel keeps a published decision" $ cancelRefund True False (Just NoRefund) @?= Right (Just NoRefund)
        ],
      testGroup
        "driver cancel reason"
        [ testCase "missing" $ requireReason Nothing @?= Left CancelReasonRequired,
          testCase "empty" $ requireReason (Just "") @?= Left CancelReasonRequired,
          testCase "blank" $ requireReason (Just "  ") @?= Left CancelReasonRequired,
          testCase "trimmed" $ requireReason (Just " rider not at stop ") @?= Right "rider not at stop"
        ]
    ]
