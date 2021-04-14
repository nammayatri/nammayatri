module Flow.RideAPI.EndRide (endRideTests) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (throwError)
import Data.List (isSubsequenceOf)
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.EndRide as Handle
import Servant.Server (ServerError)
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import Types.Error
import Utils.APIError (mustBeErrorCode)
import Utils.SilentLogger ()

endRideTests :: TestTree
endRideTests =
  testGroup
    "Ending ride"
    [ testGroup
        "Successful"
        [ successfulEndByDriver
        ],
      testGroup
        "Failing"
        [ failedEndRequestedByWrongDriver,
          failedEndRequestedNotByDriver,
          failedEndWhenRideStatusIsWrong,
          failedEndNonexistentRide,
          failedEndNonexistentDriver
        ]
    ]

handle :: Handle.ServiceHandle IO
handle =
  Handle.ServiceHandle
    { findPersonById = \case
        Id "1" -> pure Fixtures.defaultDriver
        Id "2" -> pure $ Fixtures.defaultDriver {Person._id = "2"}
        Id "admin" -> pure Fixtures.defaultAdmin
        _ -> throwError PersonDoesNotExist,
      findPIById = \piId -> pure . Right $ case piId of
        Id "search" -> Just searchProductInstance
        Id "ride" -> Just rideProductInstance
        Id "tracker" -> Just trackerProductInstance
        Id "completed_ride" -> Just rideProductInstance {PI._status = PI.COMPLETED}
        _ -> Nothing,
      findAllPIByParentId = \case
        Id "search" -> pure [rideProductInstance, trackerProductInstance]
        _ -> pure [],
      findCaseByIdAndType = \caseIds caseType ->
        if caseIds `isSubsequenceOf` ["search", "ride", "tracker"]
          then case caseType of
            Case.LOCATIONTRACKER -> pure trackerCase
            Case.RIDEORDER -> pure rideCase
            _ -> throwError CaseNotFound
          else throwError CaseNotFound,
      notifyUpdateToBAP = \_ _ _ -> pure (),
      endRideTransaction = \_ _ _ _ -> pure ()
    }

endRide ::
  Id Person.Person ->
  Id PI.ProductInstance ->
  IO (Either ServerError APISuccess.APISuccess)
endRide requestorId rideId = try $ Handle.endRideHandler handle requestorId rideId

rideProductInstance :: PI.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { PI._status = PI.INPROGRESS,
      PI._caseId = "ride",
      PI._parentId = Just "ride"
    }

searchProductInstance :: PI.ProductInstance
searchProductInstance =
  Fixtures.defaultProductInstance
    { PI._id = "search",
      PI._caseId = "search",
      PI._type = Case.RIDESEARCH,
      PI._status = PI.INPROGRESS
    }

trackerProductInstance :: PI.ProductInstance
trackerProductInstance =
  Fixtures.defaultProductInstance
    { PI._id = "tracker",
      PI._caseId = "tracker",
      PI._type = Case.LOCATIONTRACKER,
      PI._status = PI.INPROGRESS
    }

trackerCase :: Case.Case
trackerCase =
  Fixtures.defaultCase
    { Case._id = "tracker",
      Case._type = Case.LOCATIONTRACKER,
      Case._status = Case.INPROGRESS
    }

rideCase :: Case.Case
rideCase =
  Fixtures.defaultCase
    { Case._id = "ride",
      Case._type = Case.RIDEORDER,
      Case._status = Case.INPROGRESS
    }

successfulEndByDriver :: TestTree
successfulEndByDriver =
  testCase "Requested by correct driver" $
    endRide "1" "ride" >>= (@?= Right APISuccess.Success)

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    endRide "2" "ride" >>= mustBeErrorCode NotAnExecutor

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    endRide "admin" "ride" >>= mustBeErrorCode AccessDenied

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "Ride has wrong status" $
    endRide "1" "completed_ride" >>= mustBeErrorCode PIInvalidStatus

failedEndNonexistentRide :: TestTree
failedEndNonexistentRide =
  testCase "Ride does not exist" $
    endRide "1" "nonexistent_ride" >>= mustBeErrorCode PIInvalidId

failedEndNonexistentDriver :: TestTree
failedEndNonexistentDriver =
  testCase "Driver does not exist" $
    endRide "nonexistent_driver" "ride" >>= mustBeErrorCode PersonDoesNotExist
