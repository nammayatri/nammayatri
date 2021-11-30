module Flow.RideAPI.EndRide (endRideTests) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Data.List (isSubsequenceOf)
import EulerHS.Prelude
import qualified Fixtures
import Product.FareCalculator.Flow
import qualified Product.RideAPI.Handlers.EndRide as Handle
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as PI
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SearchRequest
import qualified Types.Storage.Vehicle as Veh
import Utils.Common (throwError)
import Utils.SilentLogger ()

endRideTests :: TestTree
endRideTests =
  testGroup
    "Ending ride"
    [ testGroup
        "Successful"
        [ successfulEndByDriver
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
    { findPersonById = \case
        Id "1" -> pure $ Just Fixtures.defaultDriver
        Id "2" -> pure . Just $ Fixtures.defaultDriver{id = "2"}
        Id "admin" -> pure $ Just Fixtures.defaultAdmin
        _ -> throwError PersonDoesNotExist,
      findRideBookingById = \rbId -> pure $ case rbId of
        Id "rideBooking" -> Just rideBooking
        _ -> Nothing,
      findRideById = \rideId -> pure $ case rideId of
        Id "ride" -> Just ride
        Id "completed_ride" -> Just ride{status = Ride.COMPLETED}
        _ -> Nothing,
      findSearchRequestById = \searchRequestId ->
        if searchRequestId == "search"
          then pure $ Just searchRequest
          else throwError SearchRequestNotFound,
      notifyCompleteToBAP = \_ _ -> pure (),
      endRideTransaction = \_ _ _ -> pure (),
      calculateFare = \_ _ _ _ ->
        return $
          FareParameters
            { baseFare = 100,
              distanceFare = 0,
              nightShiftRate = 0,
              discount = Nothing
            },
      recalculateFareEnabled = pure False,
      putDiffMetric = \_ _ -> pure ()
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

rideBooking :: SRB.RideBooking
rideBooking =
  Fixtures.defaultRideBooking
    { SRB.id = Id "rideBooking",
      SRB.status = SRB.TRIP_ASSIGNED,
      SRB.quoteId = Id "search"
    }

searchQuote :: PI.Quote
searchQuote =
  Fixtures.defaultQuote
    { PI.id = "search",
      PI.requestId = "search"
    }

searchRequest :: SearchRequest.SearchRequest
searchRequest =
  Fixtures.defaultSearchRequest
    { SearchRequest.id = "search",
      SearchRequest.providerId = "someOrg"
    }

successfulEndByDriver :: TestTree
successfulEndByDriver =
  testCase "Requested by correct driver" $
    endRide "1" "ride" `shouldReturn` APISuccess.Success

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
    endRide "1" "nonexistent_ride" `shouldThrow` (== RideDoesNotExist)

failedEndNonexistentDriver :: TestTree
failedEndNonexistentDriver =
  testCase "A driver does not even exist" $
    endRide "nonexistent_driver" "ride" `shouldThrow` (== PersonDoesNotExist)
