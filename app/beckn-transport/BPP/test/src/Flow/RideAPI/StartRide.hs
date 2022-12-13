module Flow.RideAPI.StartRide (startRide) where

import Beckn.External.Maps.Types
import Beckn.Types.Id
import Domain.Action.UI.Ride.StartRide as StartRide
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as Ride
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
    { findBookingById = \rbId ->
        pure $
          if rbId == Id "1"
            then Just booking
            else Nothing,
      findLocationByDriverId = \_driverId -> pure $ Just Fixtures.defaultDriverLocation,
      startRideAndUpdateLocation = \_driverId _rideId _bookingId _pt -> pure (),
      notifyBAPRideStarted = \_booking _ride -> pure (),
      rateLimitStartRide = \_driverId _rideId -> pure (),
      initializeDistanceCalculation = \_rideId _personId _pt -> pure ()
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

startRide :: TestTree
startRide =
  testGroup
    "Starting ride"
    [ successfulStartByDriver,
      successfulStartByDashboard,
      successfulStartByDashboardWithoutPoint,
      failedStartRequestedByDriverNotAnOrderExecutor,
      failedStartRequestedNotByDriver,
      failedStartRequestedByAnotherMerchantDashboard,
      failedStartWhenQuoteStatusIsWrong,
      failedStartWithWrongOTP
    ]

runDriverHandler :: StartRide.ServiceHandle IO -> Ride.Ride -> DriverStartRideReq -> IO ()
runDriverHandler sHandle sRide = StartRide.startRideHandler sHandle sRide . StartRide.DriverReq

runDashboardHandler :: StartRide.ServiceHandle IO -> Ride.Ride -> DashboardStartRideReq -> IO ()
runDashboardHandler sHandle sRide = StartRide.startRideHandler sHandle sRide . StartRide.DashboardReq

testDriverStartRideReq :: DriverStartRideReq
testDriverStartRideReq =
  DriverStartRideReq
    { rideOtp = "otp",
      point = LatLong 10 10,
      requestor = Fixtures.defaultDriver
    }

testDashboardStartRideReq :: DashboardStartRideReq
testDashboardStartRideReq =
  DashboardStartRideReq
    { point = Just $ LatLong 10 10,
      merchantId = Fixtures.defaultMerchantId
    }

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    runDriverHandler handle ride testDriverStartRideReq
      `shouldReturn` ()

successfulStartByDashboard :: TestTree
successfulStartByDashboard =
  testCase "Start successfully if requested by dashboard" $ do
    runDashboardHandler handle ride testDashboardStartRideReq
      `shouldReturn` ()

successfulStartByDashboardWithoutPoint :: TestTree
successfulStartByDashboardWithoutPoint =
  testCase "Start successfully if requested by dashboard without start point" $ do
    runDashboardHandler handle ride testStartRideReqCase
      `shouldReturn` ()
  where
    testStartRideReqCase = testDashboardStartRideReq{point = Nothing}

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    runDriverHandler handle ride testStartRideReqCase
      `shouldThrow` (== NotAnExecutor)
  where
    testStartRideReqCase = testDriverStartRideReq{requestor = Fixtures.anotherDriver}

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    runDriverHandler handle ride testStartRideReqCase
      `shouldThrow` (== AccessDenied)
  where
    testStartRideReqCase = testDriverStartRideReq{requestor = Fixtures.defaultAdmin}

failedStartRequestedByAnotherMerchantDashboard :: TestTree
failedStartRequestedByAnotherMerchantDashboard = do
  testCase "Fail ride starting if requested by another merchant dashboard" $ do
    runDashboardHandler handle ride testStartRideReqCase
      `shouldThrow` (== RideDoesNotExist ride.id.getId)
  where
    testStartRideReqCase = testDashboardStartRideReq{merchantId = Fixtures.anotherMerchantId}

failedStartWhenQuoteStatusIsWrong :: TestTree
failedStartWhenQuoteStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    runDriverHandler handle completeRide testDriverStartRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    completeRide = ride{status = Ride.COMPLETED}

wrongOtpReq :: DriverStartRideReq
wrongOtpReq = testDriverStartRideReq {rideOtp = "otp2"}

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runDriverHandler handle ride wrongOtpReq
      `shouldThrow` (== IncorrectOTP)
