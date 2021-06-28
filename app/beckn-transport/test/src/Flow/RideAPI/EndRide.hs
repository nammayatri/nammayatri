module Flow.RideAPI.EndRide (endRideTests) where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.List (isSubsequenceOf)
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.EndRide as Handle
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import Types.Error
import Utils.Common (throwError)
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
        Id "2" -> pure $ Fixtures.defaultDriver{id = "2"}
        Id "admin" -> pure Fixtures.defaultAdmin
        _ -> throwError PersonDoesNotExist,
      findPIById = \piId -> pure $ case piId of
        Id "search" -> Just searchProductInstance
        Id "ride" -> Just rideProductInstance
        Id "tracker" -> Just trackerProductInstance
        Id "completed_ride" -> Just rideProductInstance {PI.status = PI.COMPLETED}
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
  IO APISuccess.APISuccess
endRide = Handle.endRideHandler handle

rideProductInstance :: PI.ProductInstance
rideProductInstance =
  Fixtures.defaultProductInstance
    { PI.status = PI.INPROGRESS,
      PI.caseId = "ride",
      PI.parentId = Just "ride"
    }

searchProductInstance :: PI.ProductInstance
searchProductInstance =
  Fixtures.defaultProductInstance
    { PI.id = "search",
      PI.caseId = "search",
      PI._type = Case.RIDESEARCH,
      PI.status = PI.INPROGRESS
    }

trackerProductInstance :: PI.ProductInstance
trackerProductInstance =
  Fixtures.defaultProductInstance
    { PI.id = "tracker",
      PI.caseId = "tracker",
      PI._type = Case.LOCATIONTRACKER,
      PI.status = PI.INPROGRESS
    }

trackerCase :: Case.Case
trackerCase =
  Fixtures.defaultCase
    { Case.id = "tracker",
      Case._type = Case.LOCATIONTRACKER,
      Case.status = Case.INPROGRESS
    }

rideCase :: Case.Case
rideCase =
  Fixtures.defaultCase
    { Case.id = "ride",
      Case._type = Case.RIDEORDER,
      Case.status = Case.INPROGRESS
    }

successfulEndByDriver :: TestTree
successfulEndByDriver =
  testCase "Requested by correct driver" $
    endRide "1" "ride" `shouldReturn` APISuccess.Success

failedEndRequestedByWrongDriver :: TestTree
failedEndRequestedByWrongDriver =
  testCase "Requested by wrong driver" $
    endRide "2" "ride" `shouldThrow` (== NotAnExecutor)

failedEndRequestedNotByDriver :: TestTree
failedEndRequestedNotByDriver =
  testCase "Requested not by driver" $
    endRide "admin" "ride" `shouldThrow` (== AccessDenied)

failedEndWhenRideStatusIsWrong :: TestTree
failedEndWhenRideStatusIsWrong =
  testCase "A ride has wrong status" $
    endRide "1" "completed_ride" `shouldThrow` (\(PIInvalidStatus _) -> True)

failedEndNonexistentRide :: TestTree
failedEndNonexistentRide =
  testCase "A ride does not even exist" $
    endRide "1" "nonexistent_ride" `shouldThrow` (== PIDoesNotExist)

failedEndNonexistentDriver :: TestTree
failedEndNonexistentDriver =
  testCase "A driver does not even exist" $
    endRide "nonexistent_driver" "ride" `shouldThrow` (== PersonDoesNotExist)
