module Flow.RideAPI.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Action.UI.Ride.StartRide as StartRide
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Fixtures
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findById = \_personid -> pure $ Just Fixtures.defaultDriver,
      findBookingById = \rbId ->
        pure $
          if rbId == Id "1"
            then Just booking
            else Nothing,
      findRideById = \rideId ->
        pure $
          if rideId == Id "1"
            then Just ride
            else Nothing,
      startRide = \_ _ _ -> pure (),
      notifyBAPRideStarted = \_booking _ride -> pure (),
      rateLimitStartRide = \_driverId _rideId -> pure ()
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.status = Ride.NEW,
      Ride.bookingId = "1",
      Ride.otp = "otp"
    }

booking :: SRB.Booking
booking =
  Fixtures.defaultBooking
    { SRB.status = SRB.CONFIRMED
    }

searchRequest :: SearchRequest.SearchRequest
searchRequest =
  Fixtures.defaultSearchRequest
    { SearchRequest.id = "1"
    }

startRide :: TestTree
startRide =
  testGroup
    "Starting ride"
    [ successfulStartByDriver,
      failedStartRequestedByDriverNotAnOrderExecutor,
      failedStartRequestedNotByDriver,
      failedStartWhenQuoteStatusIsWrong,
      failedStartWithWrongOTP
    ]

runHandler :: StartRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> Text -> IO APISuccess.APISuccess
runHandler = StartRide.startRideHandler

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    runHandler handle "1" "1" "otp"
      `shouldReturn` APISuccess.Success

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    runHandler handleCase "2" "1" "otp"
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase =
      handle
        { StartRide.findById = \_personId ->
            pure $
              Just
                Fixtures.defaultDriver{id = "2"
                                      }
        }

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (== AccessDenied)
  where
    handleCase =
      handle
        { StartRide.findById = \_personId ->
            pure $
              Just
                Fixtures.defaultDriver{role = Person.ADMIN
                                      }
        }

failedStartWhenQuoteStatusIsWrong :: TestTree
failedStartWhenQuoteStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    handleCase =
      handle
        { StartRide.findRideById = \_rideId ->
            pure $
              Just
                ride{status = Ride.COMPLETED
                    }
        }

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runHandler handle "1" "1" "otp2"
      `shouldThrow` (== IncorrectOTP)
