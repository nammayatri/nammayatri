{-# OPTIONS_GHC -Wno-deprecations #-}

module Flow.RideAPI.EndRide (endRideTests) where

import Beckn.External.Maps.Types
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.Ride.EndRide as Handle
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude
import qualified Fixtures
import SharedLogic.FareCalculator.OneWayFareCalculator.Calculator
import SharedLogic.FareCalculator.RentalFareCalculator.Calculator
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.SilentLogger ()

endRideTests :: TestTree
endRideTests =
  testGroup
    "Ending ride"
    [ testGroup
        "Successful"
        [ successfulEndByDriver,
          successfulEndRental,
          locationUpdatesFailure,
          locationUpdatesSuccess
        ],
      testGroup
        "Failing"
        [ failedEndRequestedByWrongDriver,
          failedEndRequestedNotByDriver,
          failedEndWhenRideStatusIsWrong,
          failedEndNonexistentRide
        ]
    ]

handle :: Handle.ServiceHandle IO
handle =
  Handle.ServiceHandle
    { requestor = Fixtures.defaultDriver,
      findBookingById = \rbId -> pure $ case rbId of
        Id "booking" -> Just booking
        Id "rentalBooking" -> Just rentalBooking
        _ -> Nothing,
      findRideById = \rideId -> pure $ case rideId of
        Id "ride" -> Just ride
        Id "completed_ride" -> Just ride{status = Ride.COMPLETED}
        Id "rentalRide" -> Just rentalRide
        _ -> Nothing,
      notifyCompleteToBAP = \_ _ _ -> pure (),
      endRideTransaction = \_ _ _ -> pure (),
      calculateFare = \_ _ _ ->
        return $
          OneWayFareParameters
            { baseFare = 100,
              distanceFare = 0,
              nightShiftRate = 0,
              discount = Nothing
            },
      calculateRentalFare = \_ _ _ _ ->
        pure $
          RentalFareParameters
            { baseFare = 100,
              extraDistanceFare = 0,
              extraTimeFare = 0,
              nextDaysFare = Nothing,
              discount = Nothing,
              farePolicy = Fixtures.defaultFarePolicy
            },
      buildOneWayFareBreakups = \_ _ -> pure [],
      buildRentalFareBreakups = \_ _ -> pure [],
      recalculateFareEnabled = pure False,
      putDiffMetric = \_ _ -> pure (),
      findDriverLoc = pure Nothing,
      finalDistanceCalculation = \_ -> pure (),
      getRentalFarePolicy = undefined, -- not required for current test cases
      isDistanceCalculationFailed = pure False,
      getDefaultPickupLocThreshold = pure 500,
      getDefaultDropLocThreshold = pure 500,
      getDefaultRideTravelledDistanceThreshold = pure 700,
      getDefaultRideTimeEstimatedThreshold = pure 900,
      findConfigByKey = \_ -> pure Nothing
    }

testEndRideReq :: EndRideReq
testEndRideReq =
  EndRideReq
    { point = LatLong 10 10
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.id = "ride",
      Ride.status = Ride.INPROGRESS,
      Ride.bookingId = Id "booking"
    }

rentalRide :: Ride.Ride
rentalRide =
  ride
    { Ride.id = "rentalRide",
      Ride.bookingId = Id "rentalBooking"
    }

booking :: SRB.Booking
booking =
  Fixtures.defaultBooking
    { SRB.id = Id "booking",
      SRB.status = SRB.TRIP_ASSIGNED
    }

rentalBooking :: SRB.Booking
rentalBooking = do
  let details =
        SRB.RentalBookingDetails
          { SRB.rentalFarePolicyId = Id "rentalFarePolicy"
          }
  booking
    { SRB.id = Id "rentalBooking",
      SRB.bookingDetails = SRB.RentalDetails details
    }

successfulEndByDriver :: TestTree
successfulEndByDriver =
  testCase "Requested by correct driver" $
    Handle.endRideHandler handle "ride" testEndRideReq `shouldReturn` APISuccess.Success

successfulEndRental :: TestTree
successfulEndRental =
  testCase "Requested for rentals by correct driver" $
    Handle.endRideHandler handle "rentalRide" testEndRideReq `shouldReturn` APISuccess.Success

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    Handle.endRideHandler modHandle "ride" testEndRideReq `shouldThrow` (== NotAnExecutor)
  where
    modHandle = handle{requestor = Fixtures.defaultDriver{id = "2"}}

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    Handle.endRideHandler modHandle "ride" testEndRideReq `shouldThrow` (== AccessDenied)
  where
    modHandle = handle{requestor = Fixtures.defaultAdmin}

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "A ride has wrong status" $
    Handle.endRideHandler handle "completed_ride" testEndRideReq `shouldThrow` (\(RideInvalidStatus _) -> True)

failedEndNonexistentRide :: TestTree
failedEndNonexistentRide =
  testCase "A ride does not even exist" $
    Handle.endRideHandler handle rideId testEndRideReq `shouldThrow` (== RideDoesNotExist rideId.getId)
  where
    rideId = "nonexistent_ride"

-----

lufEstFare, lufEstTotalFare, lufActFare, lufActTotalFare :: Money
lufEstFare = 389
lufEstTotalFare = 276
lufActFare = 600
lufActTotalFare = 500

lufBooking :: SRB.Booking
lufBooking = booking {SRB.estimatedFare = lufEstFare, SRB.estimatedTotalFare = lufEstTotalFare}

locationUpdatesFailureHandle :: Handle.ServiceHandle IO
locationUpdatesFailureHandle =
  handle
    { findBookingById = \rbId -> pure $ case rbId of
        Id "booking" -> Just lufBooking
        _ -> Nothing,
      findRideById = \rideId -> pure $ case rideId of
        Id "ride" -> Just ride
        _ -> Nothing,
      isDistanceCalculationFailed = pure True,
      notifyCompleteToBAP = \_ rd _ -> checkRide rd,
      endRideTransaction = \_ rd _ -> checkRide rd,
      recalculateFareEnabled = pure True,
      calculateFare = \_ _ _ -> do
        return $
          OneWayFareParameters
            { baseFare = lufActFare,
              distanceFare = 0,
              nightShiftRate = 1,
              discount = Just $ lufActFare - lufActTotalFare
            }
    }
  where
    checkRide :: Ride.Ride -> IO ()
    checkRide rd =
      when (rd.fare /= Just lufEstFare || rd.totalFare /= Just lufEstTotalFare) $
        throwError $ InternalError "expected estimated fares as final fares"

locationUpdatesSuccessHandle :: Handle.ServiceHandle IO
locationUpdatesSuccessHandle =
  locationUpdatesFailureHandle
    { isDistanceCalculationFailed = pure False,
      notifyCompleteToBAP = \_ rd _ -> checkRide rd,
      endRideTransaction = \_ rd _ -> checkRide rd
    }
  where
    checkRide :: Ride.Ride -> IO ()
    checkRide rd =
      when (rd.fare /= Just lufActFare || rd.totalFare /= Just lufActTotalFare) $
        throwError $ InternalError $ "expected actual fares as final fares; fare = " <> show rd.fare <> ", totalFare = " <> show rd.totalFare

locationUpdatesFailure :: TestTree
locationUpdatesFailure =
  testCase "Return estimated fare when failed to calculate actual distance" $
    void $ Handle.endRideHandler locationUpdatesFailureHandle rideId testEndRideReq
  where
    rideId = "ride"

locationUpdatesSuccess :: TestTree
locationUpdatesSuccess =
  testCase "Return actual fare when succeeded to calculate actual distance" $
    void $ Handle.endRideHandler locationUpdatesSuccessHandle rideId testEndRideReq
  where
    rideId = "ride"
