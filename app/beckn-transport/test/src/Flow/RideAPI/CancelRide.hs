module Flow.RideAPI.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import Control.Monad.Identity
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.CancelRide as CancelRide
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.Ride as RideAPI
import Types.App
import Types.Error
import Types.Storage.CancellationReason
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.Ride as Ride
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findRideById = \_rideId -> pure $ Just ride,
      findPersonById = \_personid -> pure $ Just Fixtures.defaultDriver,
      cancelRide = \_rideReq _requestedByAdmin -> pure ()
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.status = Ride.CONFIRMED
    }

cancelRide :: TestTree
cancelRide =
  testGroup
    "Ride cancellation"
    [ successfulCancellationByDriver,
      successfulCancellationByAdmin,
      successfulCancellationWithoutDriverByAdmin,
      failedCancellationByAnotherDriver,
      failedCancellationWithoutDriverByDriver,
      failedCancellationWhenProductInstanceStatusIsWrong
    ]

runHandler :: CancelRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> RideAPI.CancelRideReq -> IO APISuccess.APISuccess
runHandler = CancelRide.cancelRideHandler

someCancelRideReq :: RideAPI.CancelRideReq
someCancelRideReq =
  RideAPI.CancelRideReq (CancellationReasonCode "OTHER") $ Just "Your car is not flying."

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
    handleCase = handle {CancelRide.findPersonById = \personId -> pure $ Just admin}
    admin =
      Fixtures.defaultDriver{id = Id "adminId",
                             role = Person.ADMIN
                            }

successfulCancellationWithoutDriverByAdmin :: TestTree
successfulCancellationWithoutDriverByAdmin =
  testCase "Cancel successfully if ride has no driver but requested by admin" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldReturn` APISuccess.Success
  where
    handleCase =
      handle
        { CancelRide.findRideById = \rideId -> pure $ Just piWithoutDriver,
          CancelRide.findPersonById = \personId -> pure $ Just admin
        }
    piWithoutDriver = ride {Ride.personId = Nothing}
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
    handleCase = handle {CancelRide.findPersonById = \personId -> pure $ Just driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver{id = Id "driverNotExecutorId"}

failedCancellationWithoutDriverByDriver :: TestTree
failedCancellationWithoutDriverByDriver =
  testCase "Fail cancellation if ride has no driver and requested by driver" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (== RideFieldNotPresent "person")
  where
    handleCase = handle {CancelRide.findRideById = \rideId -> pure $ Just piWithoutDriver}
    piWithoutDriver = ride {Ride.personId = Nothing}

failedCancellationWhenProductInstanceStatusIsWrong :: TestTree
failedCancellationWhenProductInstanceStatusIsWrong =
  testCase "Fail cancellation if product instance has inappropriate ride status" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (\(PIInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findRideById = \rideId -> pure $ Just completedPI}
    completedPI = ride{status = Ride.COMPLETED}
