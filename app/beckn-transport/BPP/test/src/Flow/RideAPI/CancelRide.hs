module Flow.RideAPI.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Domain.Action.UI.Ride.CancelRide as CancelRide
import Domain.Types.CancellationReason
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
      findById = \_personid -> pure $ Just Fixtures.defaultDriver,
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
      failedCancellationByAnotherDriver,
      failedCancellationWhenQuoteStatusIsWrong
    ]

runHandler :: CancelRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> CancelRide.CancelRideReq -> IO APISuccess.APISuccess
runHandler = CancelRide.cancelRideHandler

someCancelRideReq :: CancelRide.CancelRideReq
someCancelRideReq =
  CancelRide.CancelRideReq (CancellationReasonCode "OTHER") $ Just "Your car is not flying."

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $ do
    runHandler handle (Id "1") "1" someCancelRideReq
      `shouldReturn` APISuccess.Success

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldReturn` APISuccess.Success
  where
    handleCase = handle {CancelRide.findById = \_personId -> pure $ Just admin}
    admin =
      Fixtures.defaultDriver{id = Id "adminId",
                             role = Person.ADMIN
                            }

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $ do
    runHandler handleCase (Id "driverNotExecutorId") "1" someCancelRideReq
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase = handle {CancelRide.findById = \_personId -> pure $ Just driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver{id = Id "driverNotExecutorId"}

failedCancellationWhenQuoteStatusIsWrong :: TestTree
failedCancellationWhenQuoteStatusIsWrong =
  testCase "Fail cancellation if ride has inappropriate ride status" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findRideById = \_rideId -> pure $ Just completedPI}
    completedPI = ride{status = Ride.COMPLETED}
