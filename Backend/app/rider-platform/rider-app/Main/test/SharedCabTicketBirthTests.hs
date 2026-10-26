{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabTicketBirthTests (tests) where

import "rider-app" Beckn.ACL.FRFS.Utils (checkedInBirthStatus)
import qualified "beckn-spec" BecknV2.FRFS.Enums as Spec
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as DFRFSTicket
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "checked-in (scan/spot) birth status of an UNCLAIMED ticket"
    -- A ticket check-marks at birth only for a BUS, where the conductor's single scan validates the booking. A shared-cab spot
    -- booking is a walk-up, not a scan, so the ticket must start ACTIVE, not USED.
    [ testCase "SHARED_CAB tier on a BUS feed: stays ACTIVE" $
        checkedInBirthStatus (Just Spec.SHARED_CAB) Spec.BUS @?= DFRFSTicket.ACTIVE,
      testCase "ordinary bus: scanned once = USED" $
        checkedInBirthStatus (Just Spec.ORDINARY) Spec.BUS @?= DFRFSTicket.USED,
      testCase "no tier info on a BUS: still USED (behaviour unchanged)" $
        checkedInBirthStatus Nothing Spec.BUS @?= DFRFSTicket.USED,
      testCase "SHARED_CAB on a non-BUS feed: INPROGRESS like any non-bus scan" $
        checkedInBirthStatus (Just Spec.SHARED_CAB) Spec.METRO @?= DFRFSTicket.INPROGRESS
    ]
