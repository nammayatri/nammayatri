{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabLegStateTests (tests) where

import qualified "beckn-spec" BecknV2.FRFS.Enums as Spec
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (isInfixOf)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian, secondsToDiffTime)
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
        "B5: shared-cab leg bypasses the bus-schedule filter with the SHARED_CAB tier"
        [ testCase "SHARED_CAB agency -> SHARED_CAB tier" $ sharedCabFareTiers (Just "shillong_shared_cab:SHARED_CAB") @?= Just [Spec.SHARED_CAB],
          testCase "bus agency -> no bypass" $ sharedCabFareTiers (Just "chennai_bus:MTC") @?= Nothing,
          testCase "no agency -> no bypass" $ sharedCabFareTiers Nothing @?= Nothing
        ],
      testGroup
        "07 section 3: state from the 05 section 2 encoding"
        [ testCase "confirmed, no plate -> FINDING" $ derive (JMState.FRFSBooking DFRFSBooking.CONFIRMED) Nothing False Nothing Nothing @?= Just FINDING,
          testCase "ticket ACTIVE, no plate -> FINDING" $ derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False Nothing Nothing @?= Just FINDING,
          testCase "ticket ACTIVE, plate set, no timer -> ALLOCATED" $ derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) plate True Nothing Nothing @?= Just ALLOCATED,
          testCase "ticket ACTIVE, plate set, timer armed -> ARRIVING" $ derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) plate True Nothing (Just now) @?= Just ARRIVING,
          testCase "INPROGRESS with a live session -> BOARDED" $ derive (JMState.FRFSTicket DFRFSTicket.INPROGRESS) plate True Nothing Nothing @?= Just BOARDED,
          testCase "INPROGRESS, no session -> DEGRADED" $ derive (JMState.FRFSTicket DFRFSTicket.INPROGRESS) plate False Nothing Nothing @?= Just DEGRADED,
          testCase "ticket USED -> DROPPED" $ derive (JMState.FRFSTicket DFRFSTicket.USED) plate False Nothing Nothing @?= Just DROPPED,
          testCase "feedback pending (all USED) -> DROPPED" $ derive (JMState.Feedback JMState.FEEDBACK_PENDING) plate False Nothing Nothing @?= Just DROPPED,
          testCase "ticket CANCELLED -> CANCELLED" $ derive (JMState.FRFSTicket DFRFSTicket.CANCELLED) Nothing False Nothing Nothing @?= Just CANCELLED,
          testCase "booking CANCELLED -> CANCELLED" $ derive (JMState.FRFSBooking DFRFSBooking.CANCELLED) Nothing False Nothing Nothing @?= Just CANCELLED,
          testCase "not yet confirmed -> no block" $ derive (JMState.FRFSBooking DFRFSBooking.NEW) Nothing False Nothing Nothing @?= Nothing
        ],
      testGroup
        "R16: FINDING fallback gate (attempts OR fallbackAfterSec, whichever first)"
        [ testCase "no plate, gate untripped -> FINDING" $
            derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False (Just (gate 0 (-60))) Nothing @?= Just FINDING,
          testCase "no plate, attempts >= maxAttempts -> FALLBACK" $
            derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False (Just (gate 2 (-60))) Nothing @?= Just FALLBACK,
          testCase "no plate, attempts under max but findingSince past fallbackAfterSec -> FALLBACK" $
            derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False (Just (gate 0 (-700))) Nothing @?= Just FALLBACK,
          testCase "no plate, one attempt short of max and just inside fallbackAfterSec -> still FINDING" $
            derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) Nothing False (Just (gate 1 (-300))) Nothing @?= Just FINDING,
          testCase "the clock is inclusive: exactly fallbackAfterSec is due, a second less is not (the tick's push uses the same test)" $
            map (\off -> fallbackTimeElapsed now (addUTCTime off now) 600) [-600, -599] @?= [True, False],
          testCase "plate set: fallback gate is irrelevant, plate always wins" $
            derive (JMState.FRFSTicket DFRFSTicket.ACTIVE) plate True (Just (gate 2 (-700))) Nothing @?= Just ALLOCATED
        ],
      testGroup
        "I got down: which tickets go USED"
        [ testCase "waiting ticket" $ isDroppable DFRFSTicket.ACTIVE @?= True,
          testCase "boarded ticket" $ isDroppable DFRFSTicket.INPROGRESS @?= True,
          testCase "cancelled ticket stays cancelled" $ isDroppable DFRFSTicket.CANCELLED @?= False,
          testCase "used ticket untouched" $ isDroppable DFRFSTicket.USED @?= False
        ],
      testGroup
        "R55: cancelReason on the leg status"
        [ testCase "reason encodes SCREAMING like SharedCabState" $ BLC.unpack (encode NO_SHOW_CAP) @?= "\"NO_SHOW_CAP\"",
          testCase "status with a reason round-trips" $ decode (encode statusWithReason) @?= Just statusWithReason,
          testCase "a Nothing reason leaves no key (additive for older app builds)" $
            isInfixOf "cancelReason" (BLC.unpack (encode statusNoReason)) @?= False,
          testCase "a pre-R55 payload (no cancelReason key) still decodes" $
            decode "{\"state\":\"CANCELLED\",\"bookingId\":\"b1\",\"vehicleNumber\":\"ML05A1234\",\"cabsComing\":0}" @?= Just statusNoReason
        ]
    ]
  where
    derive = deriveSharedCabState now
    plate = Just "ML05A1234"
    now :: UTCTime
    now = UTCTime (fromGregorian 2026 9 29) (secondsToDiffTime 0)

    gate :: Int -> NominalDiffTime -> FallbackGate
    gate attempts offsetSec =
      FallbackGate {attempts, maxAttempts = 2, findingSince = addUTCTime offsetSec now, fallbackAfterSec = 600}

    statusNoReason :: SharedCabLegStatus
    statusNoReason =
      SharedCabLegStatus
        { state = CANCELLED,
          bookingId = "b1",
          vehicleNumber = Just "ML05A1234",
          vehicleModel = Nothing,
          driverName = Nothing,
          driverPhotoUrl = Nothing,
          etaToBoardStopSec = Nothing,
          etaToDropStopSec = Nothing,
          cabsComing = 0,
          boardDeadlineSec = Nothing,
          cancelReason = Nothing
        }

    statusWithReason :: SharedCabLegStatus
    statusWithReason = statusNoReason {cancelReason = Just NO_SHOW_CAP}
