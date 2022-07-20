module Flow.RideAPI.EndRide (endRideTests) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Action.UI.Ride.EndRide as Handle
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude
import qualified Fixtures
import SharedLogic.FareCalculator.OneWayFareCalculator.Flow
import SharedLogic.FareCalculator.RentalFareCalculator.Flow
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import Utils.Common (throwError)
import Utils.SilentLogger ()

endRideTests :: TestTree
endRideTests =
  testGroup
    "Ending ride"
    [ testGroup
        "Successful"
        [ successfulEndByDriver,
          successfulEndRental
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
      findRideBookingById = \rbId -> pure $ case rbId of
        Id "rideBooking" -> Just rideBooking
        Id "rentalRideBooking" -> Just rentalRideBooking
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
      putDiffMetric = \_ _ -> pure (),
      findDriverLocById = \_ -> pure Nothing,
      isMarketAsMissingLocationUpdates = \_ -> pure True,
      updateLocationAllowedDelay = pure 60,
      recalcDistanceEnding = \_ -> pure ()
    }

endRide ::
  Id Person.Person ->
  Id Ride.Ride ->
  IO APISuccess.APISuccess
endRide = Handle.endRideHandler handle

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.id = "ride",
      Ride.status = Ride.INPROGRESS,
      Ride.bookingId = Id "rideBooking"
    }

rentalRide :: Ride.Ride
rentalRide =
  ride
    { Ride.id = "rentalRide",
      Ride.bookingId = Id "rentalRideBooking"
    }

rideBooking :: SRB.RideBooking
rideBooking =
  Fixtures.defaultRideBooking
    { SRB.id = Id "rideBooking",
      SRB.status = SRB.TRIP_ASSIGNED
    }

rentalRideBooking :: SRB.RideBooking
rentalRideBooking = do
  let details =
        SRB.RentalRideBookingDetails
          { SRB.rentalFarePolicyId = Id "rentalFarePolicy"
          }
  rideBooking
    { SRB.id = Id "rentalRideBooking",
      SRB.rideBookingDetails = SRB.RentalDetails details
    }

successfulEndByDriver :: TestTree
successfulEndByDriver =
  testCase "Requested by correct driver" $
    endRide "1" "ride" `shouldReturn` APISuccess.Success

successfulEndRental :: TestTree
successfulEndRental =
  testCase "Requested for rentals by correct driver" $
    endRide "1" "rentalRide" `shouldReturn` APISuccess.Success

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    endRide "2" "ride" `shouldThrow` (== NotAnExecutor)

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    endRide "admin" "ride" `shouldThrow` (== AccessDenied)

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "A ride has wrong status" $
    endRide "1" "completed_ride" `shouldThrow` (\(RideInvalidStatus _) -> True)

failedEndNonexistentRide :: TestTree
failedEndNonexistentRide =
  testCase "A ride does not even exist" $
    endRide "1" "nonexistent_ride" `shouldThrow` (== RideDoesNotExist "nonexistent_ride")

failedEndNonexistentDriver :: TestTree
failedEndNonexistentDriver =
  testCase "A driver does not even exist" $
    endRide "nonexistent_driver" "ride" `shouldThrow` (== PersonDoesNotExist "nonexistent_driver")
