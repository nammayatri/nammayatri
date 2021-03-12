module Flow.RideAPI.CancelRide where

import Beckn.Product.BusinessRule (BusinessError (..), runBR)
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Control.Monad.Identity
import EulerHS.Prelude
import qualified Fixtures as Fixtures
import qualified Product.RideAPI.Handlers.CancelRide as CancelRide
import Test.Tasty
import Test.Tasty.HUnit

handle :: CancelRide.ServiceHandle Identity
handle =
  CancelRide.ServiceHandle
    { findPIById = \_piId -> pure rideProductInstance,
      findPersonById = \_personid -> pure Fixtures.defaultDriver,
      createRideRequest = \_rideReq -> pure (),
      generateGUID = pure "",
      getCurrentTime = pure Fixtures.defaultTime
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._status = ProductInstance.INPROGRESS
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

successfulCancellationByDriver :: TestTree
successfulCancellationByDriver =
  testCase "Cancel successfully if requested by driver executor" $
    runBR (CancelRide.cancelRide handle "1" "1") @?= pure (Right APIResult.Success)

successfulCancellationByAdmin :: TestTree
successfulCancellationByAdmin =
  testCase "Cancel successfully if requested by admin" $
    runBR (CancelRide.cancelRide handleCase "1" "1") @?= pure (Right APIResult.Success)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure admin}
    admin =
      Fixtures.defaultDriver
        { Person._id = PersonId "adminId",
          Person._role = Person.ADMIN
        }

successfulCancellationWithoutDriverByAdmin :: TestTree
successfulCancellationWithoutDriverByAdmin =
  testCase "Cancel successfully if ride has no driver but requested by admin" $
    runBR (CancelRide.cancelRide handleCase "1" "1") @?= pure (Right APIResult.Success)
  where
    handleCase =
      handle
        { CancelRide.findPIById = \piId -> pure piWithoutDriver,
          CancelRide.findPersonById = \personId -> pure admin
        }
    piWithoutDriver = rideProductInstance {ProductInstance._personId = Nothing}
    admin =
      Fixtures.defaultDriver
        { Person._id = PersonId "adminId",
          Person._role = Person.ADMIN
        }

failedCancellationByAnotherDriver :: TestTree
failedCancellationByAnotherDriver =
  testCase "Fail cancellation if requested by driver not executor" $
    runBR (CancelRide.cancelRide handleCase "driverNotExecutorId" "1") @?= pure (Left error)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure driverNotExecutor}
    driverNotExecutor = Fixtures.defaultDriver {Person._id = PersonId "driverNotExecutorId"}
    error = BusinessError "NOT_AN_ORDER_EXECUTOR" "You are not an order executor."

failedCancellationByNotDriverAndNotAdmin :: TestTree
failedCancellationByNotDriverAndNotAdmin =
  testCase "Fail cancellation if requested by neither driver nor admin" $
    runBR (CancelRide.cancelRide handleCase "managerId" "1") @?= pure (Left error)
  where
    handleCase = handle {CancelRide.findPersonById = \personId -> pure manager}
    manager =
      Fixtures.defaultDriver
        { Person._id = PersonId "managerId",
          Person._role = Person.MANAGER
        }
    error = BusinessError "NOT_AN_ORDER_EXECUTOR" "You are not an order executor."

failedCancellationWithoutDriverByDriver :: TestTree
failedCancellationWithoutDriverByDriver =
  testCase "Fail cancellation if ride has no driver and requested by not an admin" $
    runBR (CancelRide.cancelRide handleCase "1" "1") @?= pure (Left error)
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure piWithoutDriver}
    piWithoutDriver = rideProductInstance {ProductInstance._personId = Nothing}
    error = BusinessError "NOT_AN_ORDER_EXECUTOR" "You are not an order executor."

failedCancellationWhenProductInstanceStatusIsWrong :: TestTree
failedCancellationWhenProductInstanceStatusIsWrong =
  testCase "Fail cancellation if product instance has inappropriate ride status" $
    runBR (CancelRide.cancelRide handleCase "1" "1") @?= pure (Left error)
  where
    handleCase = handle {CancelRide.findPIById = \piId -> pure completedPI}
    completedPI = rideProductInstance {ProductInstance._status = ProductInstance.COMPLETED}
    error = BusinessError "INVALID_PRODUCT_INSTANCE" "Invalid product instance."
