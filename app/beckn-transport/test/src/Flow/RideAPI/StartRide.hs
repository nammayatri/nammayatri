module Flow.RideAPI.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.StartRide as StartRide
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findPersonById = \_personid -> pure $ Just Fixtures.defaultDriver,
      findPIById = \piId ->
        if piId == Id "1"
          then pure $ Just rideProductInstance
          else pure $ Just searchProductInstance,
      startRide = \_piIds -> pure (),
      notifyBAPRideStarted = \_searchPi _orderPi -> pure (),
      rateLimitStartRide = \_driverId _rideId -> pure ()
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance.status = ProductInstance.CONFIRMED,
      ProductInstance.parentId = Just "2",
      ProductInstance.udf4 = Just "otp"
    }

searchProductInstance :: ProductInstance.ProductInstance
searchProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance.id = "2",
      ProductInstance._type = ProductInstance.RIDESEARCH,
      ProductInstance.status = ProductInstance.CONFIRMED
    }

searchCase :: Case.Case
searchCase =
  Fixtures.defaultCase
    { Case.id = "1",
      Case._type = Case.RIDESEARCH,
      Case.status = Case.CONFIRMED
    }

startRide :: TestTree
startRide =
  testGroup
    "Starting ride"
    [ successfulStartByDriver,
      failedStartRequestedByDriverNotAnOrderExecutor,
      failedStartRequestedNotByDriver,
      failedStartWhenProductInstanceStatusIsWrong,
      failedStartWhenRideDoesNotHaveParentProductInstance,
      failedStartWhenRideMissingOTP,
      failedStartWithWrongOTP
    ]

runHandler :: StartRide.ServiceHandle IO -> Id Person.Person -> Id ProductInstance.ProductInstance -> Text -> IO APISuccess.APISuccess
runHandler = StartRide.startRideHandler

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    runHandler handle "1" "1" "otp"
      `shouldReturn` APISuccess.Success

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    runHandler handleCase "2" "1" "otp"
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure $
              Just
                Fixtures.defaultDriver{id = "2"
                                      }
        }

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (== AccessDenied)
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure $
              Just
                Fixtures.defaultDriver{role = Person.ADMIN
                                      }
        }

failedStartWhenProductInstanceStatusIsWrong :: TestTree
failedStartWhenProductInstanceStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (\(PIInvalidStatus _) -> True)
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure $
              Just
                rideProductInstance
                  { ProductInstance.status = ProductInstance.COMPLETED
                  }
        }

failedStartWhenRideDoesNotHaveParentProductInstance :: TestTree
failedStartWhenRideDoesNotHaveParentProductInstance = do
  testCase "Fail ride starting if ride does not have parent ProductInstance" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (== PIFieldNotPresent "parent_id")
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure $
              Just
                rideProductInstance
                  { ProductInstance.parentId = Nothing
                  }
        }

failedStartWhenRideMissingOTP :: TestTree
failedStartWhenRideMissingOTP = do
  testCase "Fail ride starting if ride does not have OTP" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (== PIFieldNotPresent "udf4")
  where
    handleCase =
      handle
        { StartRide.findPIById = \piId ->
            pure $
              Just
                rideProductInstance
                  { ProductInstance.udf4 = Nothing
                  }
        }

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runHandler handle "1" "1" "otp2"
      `shouldThrow` (== IncorrectOTP)
