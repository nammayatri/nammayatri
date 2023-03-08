{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Flow.RideAPI.EndRide (endRideTests) where

import Domain.Action.UI.Ride.EndRide as Handle
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude
import qualified Fixtures
import Kernel.External.Maps.Types
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
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
          successfulEndByDashboard,
          successfulEndByDashboardWithoutPoint,
          locationUpdatesFailure,
          locationUpdatesSuccess
        ],
      testGroup
        "Failing"
        [ failedEndRequestedByWrongDriver,
          failedEndRequestedNotByDriver,
          failedEndRequestednByAnotherMerchantDashboard,
          failedEndWhenRideStatusIsWrong
        ]
    ]

handle :: Handle.ServiceHandle IO
handle =
  Handle.ServiceHandle
    { fetchRide = return ride,
      findBookingById = \rbId -> pure $ case rbId of
        Id "booking" -> Just booking
        Id "rentalBooking" -> Just rentalBooking
        _ -> Nothing,
      notifyCompleteToBAP = \_ _ _ -> pure (),
      endRideTransaction = \_ _ _ _ -> pure (),
      calculateFare = \_ _ _ ->
        return $
          OneWayFareParameters
            { baseFare = 100,
              distanceFare = 0,
              nightShiftRate = 0,
              waitingChargePerMin = Just 1,
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
      findDriverLoc = \_ -> pure $ Just Fixtures.defaultDriverLocation,
      finalDistanceCalculation = \_ _ _ -> pure (),
      getRentalFarePolicy = undefined, -- not required for current test cases
      isDistanceCalculationFailed = \_ -> pure False,
      getDefaultConfig = pure Fixtures.defaultEndRideConfig,
      getConfig = pure Fixtures.defaultTransporterConfig,
      whenWithLocationUpdatesLock = \_driverId action -> action,
      getDeviationDistances = \_ _ ->
        pure $
          GetDistanceResp
            { origin = LatLong 10 1,
              destination = LatLong 10 1,
              distance = 100,
              duration = 30,
              status = "OK"
            }
    }

testDriverEndRideReq :: DriverEndRideReq
testDriverEndRideReq =
  DriverEndRideReq
    { point = LatLong 10 10,
      requestor = Fixtures.defaultDriver
    }

testDashboardEndRideReq :: DashboardEndRideReq
testDashboardEndRideReq =
  DashboardEndRideReq
    { point = Just $ LatLong 10 10,
      merchantId = Fixtures.defaultMerchantId
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
    driverEndRide handle ride.id testDriverEndRideReq `shouldReturn` Success

successfulEndRental :: TestTree
successfulEndRental =
  testCase "Requested for rentals by correct driver" $
    driverEndRide modHandle rentalRide.id testDriverEndRideReq `shouldReturn` Success
  where
    modHandle = handle{fetchRide = return rentalRide}

successfulEndByDashboard :: TestTree
successfulEndByDashboard =
  testCase "Requested by dashboard" $
    dashboardEndRide handle ride.id testDashboardEndRideReq `shouldReturn` Success

successfulEndByDashboardWithoutPoint :: TestTree
successfulEndByDashboardWithoutPoint =
  testCase "Requested by dashboard without point" $
    dashboardEndRide handle ride.id testEndRideReqCase `shouldReturn` Success
  where
    testEndRideReqCase = testDashboardEndRideReq{point = Nothing}

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    driverEndRide handle ride.id testEndRideReqCase `shouldThrow` (== NotAnExecutor)
  where
    testEndRideReqCase = testDriverEndRideReq{requestor = Fixtures.anotherDriver}

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    driverEndRide handle ride.id testEndRideReqCase `shouldThrow` (== AccessDenied)
  where
    testEndRideReqCase = testDriverEndRideReq{requestor = Fixtures.defaultAdmin}

failedEndRequestednByAnotherMerchantDashboard :: TestTree
failedEndRequestednByAnotherMerchantDashboard = do
  testCase "Fail ride ending if requested by another merchant dashboard" $ do
    dashboardEndRide handle ride.id testEndRideReqCase
      `shouldThrow` (\(RideDoesNotExist _) -> True)
  where
    testEndRideReqCase = testDashboardEndRideReq{merchantId = Fixtures.anotherMerchantId}

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "A ride has wrong status" $
    driverEndRide modHandle ride.id testDriverEndRideReq `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    modHandle = handle{fetchRide = return completeRide}
    completeRide = ride{Ride.status = Ride.COMPLETED}

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
      isDistanceCalculationFailed = \_ -> pure True,
      notifyCompleteToBAP = \_ rd _ -> checkRide rd,
      endRideTransaction = \_ _ rd _ -> checkRide rd,
      recalculateFareEnabled = pure True,
      calculateFare = \_ _ _ -> do
        return $
          OneWayFareParameters
            { baseFare = lufActFare,
              distanceFare = 0,
              nightShiftRate = 1,
              waitingChargePerMin = Just 1,
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
      endRideTransaction = \_ _ rd _ -> checkRide rd
    }
  where
    checkRide :: Ride.Ride -> IO ()
    checkRide rd =
      when (rd.fare /= Just lufEstFare || rd.totalFare /= Just lufEstTotalFare) $
        throwError $ InternalError $ "expected actual fares as final fares; fare = " <> show rd.fare <> ", totalFare = " <> show rd.totalFare

locationUpdatesFailure :: TestTree
locationUpdatesFailure =
  testCase "Return estimated fare when failed to calculate actual distance" $
    void $ driverEndRide locationUpdatesFailureHandle ride.id testDriverEndRideReq

locationUpdatesSuccess :: TestTree
locationUpdatesSuccess =
  testCase "Return actual fare when succeeded to calculate actual distance" $
    void $ driverEndRide locationUpdatesSuccessHandle ride.id testDriverEndRideReq
