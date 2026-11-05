{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabDemandTests (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified "beckn-spec" Domain.Types.SharedCabSession as SCS
import "rider-app" SharedLogic.SharedCab.Demand (StopDemand (..), candidateRiders, tallyAcrossStops, tallyDemand)
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
          @?= Map.fromList [("MALKI", StopDemand {waiting = 2, searching = 2}), ("IEWDUH", StopDemand {waiting = 0, searching = 1})],
      testCase "candidate riders: only candidate stops of the rider's route count, and interest lands on the stand" $
        candidateRiders
          (Map.fromList [("R1", [("MALKI", "r1"), ("LAITUMKHRAH", "r2")]), ("R2", [("MALKI", "r3")])])
          (Map.fromList [("R1", ["r4"])])
          [("R1", ["MALKI", "POLO"]), ("R3", ["MALKI"])]
          @?= [("MALKI", "r1"), ("MALKI", "r4")],
      testCase "route demand: a rider counts once across stops, waiting over searching" $
        tallyAcrossStops
          [("MALKI", "r1"), ("POLO", "r1"), ("POLO", "r2")]
          [("MALKI", Set.fromList ["r1", "r3"]), ("POLO", Set.fromList ["r2", "r3", "r4"])]
          @?= [ SCS.StopDemandCount {SCS.stopCode = "MALKI", SCS.waiting = Just 1, SCS.searching = Just 1},
                SCS.StopDemandCount {SCS.stopCode = "POLO", SCS.waiting = Just 1, SCS.searching = Just 1}
              ]
    ]
