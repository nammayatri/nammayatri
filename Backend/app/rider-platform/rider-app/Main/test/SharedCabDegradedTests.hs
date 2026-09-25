{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabDegradedTests (tests) where

import qualified "beckn-spec" Domain.Types.FRFSTicketBookingStatus as BookingStatus
import "beckn-spec" Domain.Types.FRFSTicketStatus (FRFSTicketStatus (..))
import "rider-app" SharedLogic.SharedCab.Degraded (planDegrade, shouldExpireDegraded)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "SharedCab degraded boarding"
    [ testGroup
        "unknown code"
        [ testCase "FINDING degrades, nothing to give back" $
            planDegrade BookingStatus.CONFIRMED Nothing [ACTIVE] @?= Just Nothing,
          testCase "allocated gives its cab back, then degrades" $
            planDegrade BookingStatus.CONFIRMED (Just "ML05A1234") [ACTIVE] @?= Just (Just "ML05A1234"),
          testCase "already riding a real cab: refused" $
            planDegrade BookingStatus.CONFIRMED (Just "ML05A1234") [INPROGRESS] @?= Nothing,
          testCase "cancelled booking: refused" $
            planDegrade BookingStatus.CANCELLED Nothing [ACTIVE] @?= Nothing,
          testCase "no ticket still held: refused" $
            planDegrade BookingStatus.CONFIRMED Nothing [USED, CANCELLED] @?= Nothing
        ],
      testGroup
        "timeout end"
        [ testCase "marker expired, no cab, still riding: ends" $
            shouldExpireDegraded Nothing [INPROGRESS] False @?= True,
          testCase "marker alive: not yet" $
            shouldExpireDegraded Nothing [INPROGRESS] True @?= False,
          testCase "boarded a real cab after degrading: that cab ends it, not the marker" $
            shouldExpireDegraded (Just "ML05A1234") [INPROGRESS] False @?= False,
          testCase "already got down: nothing to end" $
            shouldExpireDegraded Nothing [USED] False @?= False
        ]
    ]
