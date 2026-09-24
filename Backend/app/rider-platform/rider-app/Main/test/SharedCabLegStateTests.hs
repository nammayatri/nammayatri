{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabLegStateTests (tests) where

import qualified "beckn-spec" Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified "beckn-spec" Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified "rider-app" Lib.JourneyModule.State.Types as JMState
import "rider-app" SharedLogic.SharedCab.LegState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

tests :: TestTree
tests =
  testGroup
    "shared-cab leg state"
    [ testGroup
        "search gate: shared-cab leg detection"
        [ testCase "SHARED_CAB agency" $ isSharedCabAgency "shillong_shared_cab:SHARED_CAB" @?= True,
          testCase "bus agency" $ isSharedCabAgency "chennai_bus:MTC" @?= False,
          testCase "bare SHARED_CAB id" $ isSharedCabAgency "SHARED_CAB" @?= True
        ],
      testGroup
        "07 section 3: state from the 05 section 2 encoding"
        [ testCase "confirmed, no plate -> FINDING" $ derive (JMState.FRFSBooking DFRFSBooking.CONFIRMED) Nothing False @?= Just FINDING,
          testCase "ticket ACTIVE, no plate -> FINDING" $ derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False @?= Just FINDING,
          testCase "ticket ACTIVE, plate set -> ALLOCATED" $ derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) plate True @?= Just ALLOCATED,
          testCase "INPROGRESS with a live session -> BOARDED" $ derive (JMState.FRFSTicket DFRFSTicket.INPROGRESS) plate True @?= Just BOARDED,
          testCase "INPROGRESS, no session -> DEGRADED" $ derive (JMState.FRFSTicket DFRFSTicket.INPROGRESS) plate False @?= Just DEGRADED,
          testCase "ticket USED -> DROPPED" $ derive (JMState.FRFSTicket DFRFSTicket.USED) plate False @?= Just DROPPED,
          testCase "feedback pending (all USED) -> DROPPED" $ derive (JMState.Feedback JMState.FEEDBACK_PENDING) plate False @?= Just DROPPED,
          testCase "ticket CANCELLED -> CANCELLED" $ derive (JMState.FRFSTicket DFRFSTicket.CANCELLED) Nothing False @?= Just CANCELLED,
          testCase "booking CANCELLED -> CANCELLED" $ derive (JMState.FRFSBooking DFRFSBooking.CANCELLED) Nothing False @?= Just CANCELLED,
          testCase "not yet confirmed -> no block" $ derive (JMState.FRFSBooking DFRFSBooking.NEW) Nothing False @?= Nothing
        ]
    ]
  where
    derive = deriveSharedCabState
    plate = Just "ML05A1234"
