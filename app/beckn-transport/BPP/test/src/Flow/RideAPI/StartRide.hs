module Flow.RideAPI.StartRide where

import Beckn.External.Maps.Types
import Beckn.Types.Id
import Domain.Action.UI.Ride.StartRide as StartRide
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.SearchRequest as SearchRequest
import EulerHS.Prelude
import qualified Fixtures
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { requestor = Fixtures.defaultDriver,
      findBookingById = \rbId ->
        pure $
          if rbId == Id "1"
            then Just booking
            else Nothing,
      startRideAndUpdateLocation = \_rideId _bookingId _pt -> pure (),
      notifyBAPRideStarted = \_booking _ride -> pure (),
      rateLimitStartRide = \_rideId -> pure (),
      initializeDistanceCalculation = \_pt -> pure ()
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

runHandler :: StartRide.ServiceHandle IO -> Ride.Ride -> StartRideReq -> IO ()
runHandler = StartRide.startRideHandler

testStartRideReq :: StartRideReq
testStartRideReq =
  StartRideReq
    { rideOtp = "otp",
      point = LatLong 10 10
    }

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    runHandler handle ride testStartRideReq
      `shouldReturn` ()

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    runHandler handleCase ride testStartRideReq
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase =
      handle
        { StartRide.requestor =
            Fixtures.defaultDriver{id = "2"}
        }

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    runHandler handleCase ride testStartRideReq
      `shouldThrow` (== AccessDenied)
  where
    handleCase =
      handle
        { StartRide.requestor =
            Fixtures.defaultDriver{role = Person.ADMIN}
        }

failedStartWhenQuoteStatusIsWrong :: TestTree
failedStartWhenQuoteStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    runHandler handle completeRide testStartRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    completeRide = ride{status = Ride.COMPLETED}

wrongOtpReq :: StartRideReq
wrongOtpReq = testStartRideReq {rideOtp = "otp2"}

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runHandler handle ride wrongOtpReq
      `shouldThrow` (== IncorrectOTP)
