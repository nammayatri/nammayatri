module Flow.RideAPI.CancelRide where

import Beckn.Product.BusinessRule (BusinessError (..), runBR)
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Control.Monad.Identity
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.CancelRide as CancelRide
import Servant.Server (ServerError)
import Test.Tasty
import Test.Tasty.HUnit
import Utils.Common (errorCodeWhenLeft)
import Utils.GuidGenerator ()
import Utils.SilentLogger ()

handle :: CancelRide.ServiceHandle IO
handle =
  CancelRide.ServiceHandle
    { findPIById = \_piId -> pure rideProductInstance,
      findPersonById = \_personid -> pure Fixtures.defaultDriver,
      cancelRide = \_rideReq _requestedByAdmin -> pure ()
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._status = ProductInstance.CONFIRMED
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

runHandler :: CancelRide.ServiceHandle IO -> Text -> Text -> IO (Either ServerError APISuccess.APISuccess)
runHandler handle requestorId rideId = try $ CancelRide.cancelRideHandler handle requestorId rideId

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $ do
    result <- runHandler handle "1" "1"
    result @?= Right APISuccess.Success

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $ do
    result <- runHandler handleCase "1" "1"
    result @?= Right APISuccess.Success
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure admin}
    admin =
      Fixtures.defaultDriver
        { Person._id = Id "adminId",
          Person._role = Person.ADMIN
        }

successfulCancellationWithoutDriverByAdmin :: TestTree
successfulCancellationWithoutDriverByAdmin =
  testCase "Cancel successfully if ride has no driver but requested by admin" $ do
    result <- runHandler handleCase "1" "1"
    result @?= Right APISuccess.Success
  where
    handleCase =
      handle
        { CancelRide.findPIById = \piId -> pure piWithoutDriver,
          CancelRide.findPersonById = \personId -> pure admin
        }
    piWithoutDriver = rideProductInstance {ProductInstance._personId = Nothing}
    admin =
      Fixtures.defaultDriver
        { Person._id = Id "adminId",
          Person._role = Person.ADMIN
        }

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $ do
    result <- runHandler handleCase "driverNotExecutorId" "1"
    errorCodeWhenLeft result @?= Left "NOT_AN_EXECUTOR"
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver {Person._id = Id "driverNotExecutorId"}

failedCancellationByNotDriverAndNotAdmin :: TestTree
failedCancellationByNotDriverAndNotAdmin =
  testCase "Fail cancellation if requested by neither driver nor admin" $ do
    result <- runHandler handleCase "managerId" "1"
    errorCodeWhenLeft result @?= Left "ACCESS_DENIED"
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure manager}
    manager =
      Fixtures.defaultDriver
        { Person._id = Id "managerId",
          Person._role = Person.MANAGER
        }

failedCancellationWithoutDriverByDriver :: TestTree
failedCancellationWithoutDriverByDriver =
  testCase "Fail cancellation if ride has no driver and requested by driver" $ do
    result <- runHandler handleCase "1" "1"
    errorCodeWhenLeft result @?= Left "PI_INVALID_STATUS"
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure piWithoutDriver}
    piWithoutDriver = rideProductInstance {ProductInstance._personId = Nothing}

failedCancellationWhenProductInstanceStatusIsWrong :: TestTree
failedCancellationWhenProductInstanceStatusIsWrong =
  testCase "Fail cancellation if product instance has inappropriate ride status" $ do
    result <- runHandler handleCase "1" "1"
    errorCodeWhenLeft result @?= Left "PI_INVALID_STATUS"
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure completedPI}
    completedPI = rideProductInstance {ProductInstance._status = ProductInstance.COMPLETED}
