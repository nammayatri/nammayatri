{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabDemandTests (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as DFRFSTicket
import "rider-app" SharedLogic.SharedCab.Demand (StopDemand (..), tallyDemand)
import "rider-app" SharedLogic.SharedCab.LegState (seatsHeld)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "shared-cab seats and demand"
    [ testCase "seats held: tickets still held count, finished and cancelled ones don't" $
        seatsHeld [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS, DFRFSTicket.USED, DFRFSTicket.CANCELLED, DFRFSTicket.EXPIRED] @?= 2,
      testCase "demand: a searcher who booked is waiting, not searching; riders are distinct" $
        tallyDemand
          [("MALKI", "r1"), ("MALKI", "r1"), ("MALKI", "r2")]
          [("MALKI", Set.fromList ["r1", "r3", "r4"]), ("IEWDUH", Set.fromList ["r2"])]
          @?= Map.fromList [("MALKI", StopDemand {waiting = 2, searching = 2}), ("IEWDUH", StopDemand {waiting = 0, searching = 1})]
    ]
