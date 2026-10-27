{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SharedCabInvariantsTests (tests) where

import "beckn-spec" Domain.Types.FRFSTicketStatus (FRFSTicketStatus (..))
import "rider-app" SharedLogic.SharedCab.Invariants
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- | A booking allocated to a cab with its timer running: breaks no rule.
allocated :: BookingFacts
allocated =
  BookingFacts
    { confirmed = True,
      vehicleNumber = Just "ML05A1234",
      tripId = Nothing,
      vehicleTripId = Nothing,
      ticketStatuses = [ACTIVE],
      hasAllocTimer = True,
      degraded = False,
      payOnBoard = True,
      paymentRows = 0,
      seatHold = True
    }

boarded :: BookingFacts
boarded = allocated {ticketStatuses = [INPROGRESS], hasAllocTimer = False, vehicleTripId = Just "trip1"}

cab :: CabFacts
cab = CabFacts {liveSession = Just (LiveSession "trip1"), activeTripId = Just "trip1"}

tests :: TestTree
tests =
  testGroup
    "SharedCab invariants (validator layer 4)"
    [ testCase "healthy booking and cab break nothing" $
        (bookingViolations allocated, bookingViolations boarded, cabViolations cab) @?= ([], [], []),
      testCase "allocated booking without its Redis timer" $
        (allocatedHasTimer allocated {hasAllocTimer = False}, allocatedHasTimer allocated {vehicleNumber = Nothing, hasAllocTimer = False})
          @?= (Just AllocatedWithoutTimer, Nothing),
      testCase "boarded booking with no vehicleTripId, unless degraded" $
        (boardedHasTrip boarded {vehicleTripId = Nothing}, boardedHasTrip boarded {vehicleTripId = Nothing, degraded = True})
          @?= (Just BoardedWithoutTrip, Nothing),
      testCase "a booking on a cab with a ticket held must hold its seats in the seat engine; a degraded boarding holds none" $
        ( seatedHasHold allocated {seatHold = False},
          seatedHasHold boarded {seatHold = False},
          seatedHasHold allocated {seatHold = False, vehicleNumber = Nothing},
          seatedHasHold boarded {seatHold = False, degraded = True}
        )
          @?= (Just SeatedWithoutHold, Just SeatedWithoutHold, Nothing, Nothing),
      testCase "seats held for a booking that is off its cab, dropped or cancelled" $
        ( holdHasSeat allocated {vehicleNumber = Nothing},
          holdHasSeat boarded {ticketStatuses = [USED]},
          holdHasSeat allocated {confirmed = False},
          holdHasSeat allocated {seatHold = False, vehicleNumber = Nothing}
        )
          @?= (Just HoldWithoutSeat, Just HoldWithoutSeat, Just HoldWithoutSeat, Nothing),
      testCase "ACTIVE trip row and live session must match" $
        ( tripMatchesSession cab {activeTripId = Nothing},
          tripMatchesSession cab {activeTripId = Just "trip0"},
          tripMatchesSession cab {liveSession = Nothing}
        )
          @?= (Just LiveSessionWithoutTrip, Just SessionTripMismatch, Just TripWithoutLiveSession),
      testCase "SHARED_CAB booking with a bus trip_id" $
        noBusTripId allocated {tripId = Just "WB123-4"} @?= Just BusTripIdSet,
      testCase "payment order on a payOnBoard booking" $
        (noPaymentOnPayOnBoard allocated {paymentRows = 1}, noPaymentOnPayOnBoard allocated {payOnBoard = False, paymentRows = 1})
          @?= (Just PaymentOnPayOnBoard, Nothing)
    ]
