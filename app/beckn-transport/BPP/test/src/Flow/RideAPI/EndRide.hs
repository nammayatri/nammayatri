{-# OPTIONS_GHC -Wno-deprecations #-}

module Flow.RideAPI.EndRide (endRideTests) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common
import Domain.Action.UI.Ride.EndRide as Handle
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
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
          failedEndNonexistentRide,
          failedEndNonexistentDriver
        ]
    ]

handle :: Handle.ServiceHandle IO
handle =
  Handle.ServiceHandle
    { findById = \case
        Id "1" -> pure $ Just Fixtures.defaultDriver
        Id "2" -> pure . Just $ Fixtures.defaultDriver{id = "2"}
        Id "admin" -> pure $ Just Fixtures.defaultAdmin
        Id personId -> throwError (PersonDoesNotExist personId),
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
      endRideTransaction = \_ _ _ _ -> pure (),
      calculateFare = \_ _ _ _ ->
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
      putDiffMetric = \_ _ _ -> pure (),
      findDriverLocById = \_ -> pure Nothing,
      finalDistanceCalculation = \_ _ -> pure (),
      getRentalFarePolicy = undefined, -- not required for current test cases
      isDistanceCalculationFailed = \_ -> pure False,
      getDefaultPickupLocThreshold = pure 500,
      getDefaultDropLocThreshold = pure 500,
      findConfigByOrgIdAndKey = \_ _ -> pure Nothing
    }

endRideDefault ::
  Id Person.Person ->
  Id Ride.Ride ->
  EndRideReq ->
  IO APISuccess.APISuccess
endRideDefault = Handle.endRideHandler handle

endRide ::
  Handle.ServiceHandle IO ->
  Id Person.Person ->
  Id Ride.Ride ->
  EndRideReq ->
  IO APISuccess.APISuccess
endRide = Handle.endRideHandler

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
    endRideDefault "1" "ride" testEndRideReq `shouldReturn` APISuccess.Success

successfulEndRental :: TestTree
successfulEndRental =
  testCase "Requested for rentals by correct driver" $
    endRideDefault "1" "rentalRide" testEndRideReq `shouldReturn` APISuccess.Success

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    endRideDefault "2" "ride" testEndRideReq `shouldThrow` (== NotAnExecutor)

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    endRideDefault "admin" "ride" testEndRideReq `shouldThrow` (== AccessDenied)

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "A ride has wrong status" $
    endRideDefault "1" "completed_ride" testEndRideReq `shouldThrow` (\(RideInvalidStatus _) -> True)

failedEndNonexistentRide :: TestTree
failedEndNonexistentRide =
  testCase "A ride does not even exist" $
    endRideDefault "1" rideId testEndRideReq `shouldThrow` (== RideDoesNotExist rideId.getId)
  where
    rideId = "nonexistent_ride"

failedEndNonexistentDriver :: TestTree
failedEndNonexistentDriver =
  testCase "A driver does not even exist" $
    endRideDefault "nonexistent_driver" "ride" testEndRideReq `shouldThrow` (== PersonDoesNotExist "nonexistent_driver")

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
      isDistanceCalculationFailed = \_ -> pure True,
      notifyCompleteToBAP = \_ rd _ -> checkRide rd,
      endRideTransaction = \_ rd _ _ -> checkRide rd,
      recalculateFareEnabled = pure True,
      calculateFare = \_ _ _ _ -> do
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
    { isDistanceCalculationFailed = \_ -> pure False,
      notifyCompleteToBAP = \_ rd _ -> checkRide rd,
      endRideTransaction = \_ rd _ _ -> checkRide rd
    }
  where
    checkRide :: Ride.Ride -> IO ()
    checkRide rd =
      when (rd.fare /= Just lufActFare || rd.totalFare /= Just lufActTotalFare) $
        throwError $ InternalError $ "expected actual fares as final fares; fare = " <> show rd.fare <> ", totalFare = " <> show rd.totalFare

locationUpdatesFailure :: TestTree
locationUpdatesFailure =
  testCase "Return estimated fare when failed to calculate actual distance" $
    void $ endRide locationUpdatesFailureHandle "1" rideId testEndRideReq
  where
    rideId = "ride"

locationUpdatesSuccess :: TestTree
locationUpdatesSuccess =
  testCase "Return actual fare when succeeded to calculate actual distance" $
    void $ endRide locationUpdatesSuccessHandle "1" rideId testEndRideReq
  where
    rideId = "ride"
