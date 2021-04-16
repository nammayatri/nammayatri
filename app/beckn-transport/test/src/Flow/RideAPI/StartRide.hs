module Flow.RideAPI.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.StartRide as StartRide
import Test.Hspec
import Servant.Server as Serv (ServerError)
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findPersonById = \_personid -> pure Fixtures.defaultDriver,
      findPIById = \piId ->
        if piId == Id "1"
          then pure rideProductInstance
          else pure searchProductInstance,
      findPIsByParentId = \_parentId -> pure [rideProductInstance, trackerProductInstance],
      findCaseByIdsAndType = \_caseIds caseType ->
        if caseType == Case.LOCATIONTRACKER
          then pure trackerCase
          else pure rideCase,
      startRide = \_piIds _trackerCaseId _orderCaseId -> pure (),
      notifyBAPRideStarted = \_searchPi _orderPi -> pure ()
    }

rideProductInstance :: ProductInstance.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._status = ProductInstance.CONFIRMED,
      ProductInstance._parentId = Just "2",
      ProductInstance._udf4 = Just "otp"
    }

searchProductInstance :: ProductInstance.ProductInstance
searchProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._id = "2",
      ProductInstance._caseId = "2",
      ProductInstance._type = Case.RIDESEARCH,
      ProductInstance._status = ProductInstance.CONFIRMED
    }

trackerProductInstance :: ProductInstance.ProductInstance
trackerProductInstance =
  Fixtures.defaultProductInstance
    { ProductInstance._id = "3",
      ProductInstance._caseId = "3",
      ProductInstance._type = Case.LOCATIONTRACKER,
      ProductInstance._status = ProductInstance.CONFIRMED
    }

trackerCase :: Case.Case
trackerCase =
  Fixtures.defaultCase
    { Case._type = Case.LOCATIONTRACKER,
      Case._status = Case.CONFIRMED
    }

rideCase :: Case.Case
rideCase =
  Fixtures.defaultCase
    { Case._id = "2",
      Case._type = Case.RIDEORDER,
      Case._status = Case.CONFIRMED
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
            pure
              Fixtures.defaultDriver
                { Person._id = "2"
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
            pure
              Fixtures.defaultDriver
                { Person._role = Person.ADMIN
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
            pure
              rideProductInstance
                { ProductInstance._status = ProductInstance.COMPLETED
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
            pure
              rideProductInstance
                { ProductInstance._parentId = Nothing
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
            pure
              rideProductInstance
                { ProductInstance._udf4 = Nothing
                }
        }

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runHandler handle "1" "1" "otp2"
      `shouldThrow` (== IncorrectOTP)
