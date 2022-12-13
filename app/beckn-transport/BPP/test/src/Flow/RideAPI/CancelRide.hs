module Flow.RideAPI.CancelRide (cancelRide) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Action.UI.Ride.CancelRide as CancelRide
import Domain.Types.CancellationReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import EulerHS.Prelude
import qualified Fixtures
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tools.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findRideById = \_rideId -> pure $ Just ride,
      findById = \personId ->
        pure $
          find
            (\person -> person.id == personId)
            [Fixtures.defaultDriver, Fixtures.anotherDriver, Fixtures.defaultAdmin, Fixtures.anotherMerchantAdmin],
      cancelRide = \_rideReq _requestedByAdmin -> pure ()
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.status = Ride.NEW
    }

cancelRide :: TestTree
cancelRide =
  testGroup
    "Ride cancellation"
    [ successfulCancellationByDriver,
      successfulCancellationByAdmin,
      successfulCancellationByDashboard,
      failedCancellationByAnotherDriver,
      failedCancellationByAnotherAdmin,
      failedCancellationByAnotherMerchantDashboard,
      failedCancellationWhenQuoteStatusIsWrong
    ]

runDriverHandler :: CancelRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> CancelRide.CancelRideReq -> IO APISuccess.APISuccess
runDriverHandler = CancelRide.driverCancelRideHandler

runDashboardHandler :: CancelRide.ServiceHandle IO -> Id DM.Merchant -> Id Ride.Ride -> CancelRide.CancelRideReq -> IO APISuccess.APISuccess
runDashboardHandler = CancelRide.dashboardCancelRideHandler

someCancelRideReq :: CancelRide.CancelRideReq
someCancelRideReq =
  CancelRide.CancelRideReq (CancellationReasonCode "OTHER") $ Just "Your car is not flying."

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $ do
    runDriverHandler handle Fixtures.defaultDriver.id "1" someCancelRideReq
      `shouldReturn` APISuccess.Success

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $ do
    runDriverHandler handle Fixtures.defaultAdmin.id "1" someCancelRideReq
      `shouldReturn` APISuccess.Success

successfulCancellationByDashboard :: TestTree
successfulCancellationByDashboard =
  testCase "Cancel successfully if requested by dashboard" $ do
    runDashboardHandler handle Fixtures.defaultMerchantId "1" someCancelRideReq
      `shouldReturn` APISuccess.Success

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $ do
    runDriverHandler handle Fixtures.anotherDriver.id "1" someCancelRideReq
      `shouldThrow` (== NotAnExecutor)

failedCancellationByAnotherAdmin :: TestTree
failedCancellationByAnotherAdmin =
  testCase "Fail cancellation if requested by another merchant admin" $ do
    runDriverHandler handle Fixtures.anotherMerchantAdmin.id "1" someCancelRideReq
      `shouldThrow` (== RideDoesNotExist "1")

failedCancellationByAnotherMerchantDashboard :: TestTree
failedCancellationByAnotherMerchantDashboard =
  testCase "Fail cancellation if requested by another merchant dashboard" $ do
    runDashboardHandler handle Fixtures.anotherMerchantId "1" someCancelRideReq
      `shouldThrow` (== RideDoesNotExist "1")

failedCancellationWhenQuoteStatusIsWrong :: TestTree
failedCancellationWhenQuoteStatusIsWrong =
  testCase "Fail cancellation if ride has inappropriate ride status" $ do
    runDriverHandler handleCase Fixtures.defaultDriver.id "1" someCancelRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findRideById = \_rideId -> pure $ Just completedRide}
    completedRide = ride{status = Ride.COMPLETED}
