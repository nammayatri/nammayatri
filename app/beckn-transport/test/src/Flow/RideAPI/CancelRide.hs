module Flow.RideAPI.CancelRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Control.Monad.Identity
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.CancelRide as CancelRide
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import Types.Error
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findPIById = \_piId -> pure $ Just rideProductInstance,
      findPersonById = \_personid -> pure $ Just Fixtures.defaultDriver,
      cancelRide = \_rideReq _requestedByAdmin -> pure ()
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance.status = ProductInstance.CONFIRMED
    }

cancelRide :: TestTree
cancelRide =
  testGroup
    "Ride cancellation"
    [ successfulCancellationByDriver,
      successfulCancellationByAdmin,
      successfulCancellationWithoutDriverByAdmin,
      failedCancellationByAnotherDriver,
      failedCancellationByNotDriverAndNotAdmin,
      failedCancellationWithoutDriverByDriver,
      failedCancellationWhenProductInstanceStatusIsWrong
    ]

runHandler :: CancelRide.ServiceHandle IO -> Text -> Id Ride -> IO APISuccess.APISuccess
runHandler = CancelRide.cancelRideHandler

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $ do
    runHandler handle "1" "1"
      `shouldReturn` APISuccess.Success

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $ do
    runHandler handleCase "1" "1"
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
    runHandler handleCase "1" "1"
      `shouldReturn` APISuccess.Success
  where
    handleCase =
      handle
        { CancelRide.findPIById = \piId -> pure $ Just piWithoutDriver,
          CancelRide.findPersonById = \personId -> pure $ Just admin
        }
    piWithoutDriver = rideProductInstance {ProductInstance.personId = Nothing}
    admin =
      Fixtures.defaultDriver{id = Id "adminId",
                             role = Person.ADMIN
                            }

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $ do
    runHandler handleCase "driverNotExecutorId" "1"
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure $ Just driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver{id = Id "driverNotExecutorId"}

failedCancellationByNotDriverAndNotAdmin :: TestTree
failedCancellationByNotDriverAndNotAdmin =
  testCase "Fail cancellation if requested by neither driver nor admin" $ do
    runHandler handleCase "managerId" "1"
      `shouldThrow` (== AccessDenied)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure $ Just manager}
    manager =
      Fixtures.defaultDriver{id = Id "managerId",
                             role = Person.MANAGER
                            }

failedCancellationWithoutDriverByDriver :: TestTree
failedCancellationWithoutDriverByDriver =
  testCase "Fail cancellation if ride has no driver and requested by driver" $ do
    runHandler handleCase "1" "1"
      `shouldThrow` (== PIFieldNotPresent "person")
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure $ Just piWithoutDriver}
    piWithoutDriver = rideProductInstance {ProductInstance.personId = Nothing}

failedCancellationWhenProductInstanceStatusIsWrong :: TestTree
failedCancellationWhenProductInstanceStatusIsWrong =
  testCase "Fail cancellation if product instance has inappropriate ride status" $ do
    runHandler handleCase "1" "1"
      `shouldThrow` (\(PIInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure $ Just completedPI}
    completedPI = rideProductInstance {ProductInstance.status = ProductInstance.COMPLETED}
