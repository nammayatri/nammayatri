{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow.RideAPI.StartRide (startRide) where

import Domain.Action.UI.Ride.StartRide as StartRide
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude
import qualified Fixtures
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.CommonImport
import Kernel.Types.Id
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findRideById = \_rideId -> pure (Just ride),
      findBookingById = \rbId ->
        pure $
          if rbId == Id "1"
            then Just booking
            else Nothing,
      findLocationByDriverId = \_driverId -> pure $ Just Fixtures.defaultDriverLocation,
      startRideAndUpdateLocation = \_driverId _rideId _bookingId _pt -> pure (),
      notifyBAPRideStarted = \_booking _ride -> pure (),
      rateLimitStartRide = \_driverId _rideId -> pure (),
      initializeDistanceCalculation = \_rideId _personId _pt -> pure (),
      whenWithLocationUpdatesLock = \_driverId action -> action
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
    driverStartRide handle ride.id testDriverStartRideReq
      `shouldReturn` Success

successfulStartByDashboard :: TestTree
successfulStartByDashboard =
  testCase "Start successfully if requested by dashboard" $ do
    dashboardStartRide handle ride.id testDashboardStartRideReq
      `shouldReturn` Success

successfulStartByDashboardWithoutPoint :: TestTree
successfulStartByDashboardWithoutPoint =
  testCase "Start successfully if requested by dashboard without start point" $ do
    dashboardStartRide handle ride.id testStartRideReqCase
      `shouldReturn` Success
  where
    testStartRideReqCase = testDashboardStartRideReq{point = Nothing}

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    driverStartRide handle ride.id testStartRideReqCase
      `shouldThrow` (== NotAnExecutor)
  where
    testStartRideReqCase = testDriverStartRideReq{requestor = Fixtures.anotherDriver}

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    driverStartRide handle ride.id testStartRideReqCase
      `shouldThrow` (== AccessDenied)
  where
    testStartRideReqCase = testDriverStartRideReq{requestor = Fixtures.defaultAdmin}

failedStartRequestedByAnotherMerchantDashboard :: TestTree
failedStartRequestedByAnotherMerchantDashboard = do
  testCase "Fail ride starting if requested by another merchant dashboard" $ do
    dashboardStartRide handle ride.id testStartRideReqCase
      `shouldThrow` (== RideDoesNotExist ride.id.getId)
  where
    testStartRideReqCase = testDashboardStartRideReq{merchantId = Fixtures.anotherMerchantId}

failedStartWhenQuoteStatusIsWrong :: TestTree
failedStartWhenQuoteStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    driverStartRide modHandle ride.id testDriverStartRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    modHandle = handle{findRideById = \_rideId -> return $ Just completeRide}
    completeRide = ride{status = Ride.COMPLETED}

wrongOtpReq :: DriverStartRideReq
wrongOtpReq = testDriverStartRideReq {rideOtp = "otp2"}

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    driverStartRide handle ride.id wrongOtpReq
      `shouldThrow` (== IncorrectOTP)
