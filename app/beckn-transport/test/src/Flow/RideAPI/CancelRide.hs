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
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findPIById = \_piId -> pure $ Just rideProductInstance,
      findPersonById = \_personid -> pure $ Just Fixtures.defaultDriver,
      cancelRide = \_rideReq _requestedByAdmin _ -> pure ()
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
      failedCancellationWithoutDriverByDriver,
      failedCancellationWhenProductInstanceStatusIsWrong
    ]

runHandler :: CancelRide.ServiceHandle IO -> Id Person.Person -> Id Ride -> RideAPI.CancelRideReq -> IO APISuccess.APISuccess
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
    runHandler handleCase (Id "driverNotExecutorId") "1" someCancelRideReq
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure $ Just driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver{id = Id "driverNotExecutorId"}

failedCancellationWithoutDriverByDriver :: TestTree
failedCancellationWithoutDriverByDriver =
  testCase "Fail cancellation if ride has no driver and requested by driver" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (== PIFieldNotPresent "person")
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure $ Just piWithoutDriver}
    piWithoutDriver = rideProductInstance {ProductInstance.personId = Nothing}

failedCancellationWhenProductInstanceStatusIsWrong :: TestTree
failedCancellationWhenProductInstanceStatusIsWrong =
  testCase "Fail cancellation if product instance has inappropriate ride status" $ do
    runHandler handleCase (Id "1") "1" someCancelRideReq
      `shouldThrow` (\(PIInvalidStatus _) -> True)
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure $ Just completedPI}
    completedPI = rideProductInstance {ProductInstance.status = ProductInstance.COMPLETED}
