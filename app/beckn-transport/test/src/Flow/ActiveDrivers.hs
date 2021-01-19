module Flow.ActiveDrivers (runTests) where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Ratio ((%))
import Data.Time (UTCTime)
import EulerHS.Prelude
import qualified Fixtures
import Product.DriverInformation (ServiceHandle (..), handleGetAvailableDriversInfo)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.DriverInformation as API

handle :: ServiceHandle Identity
handle =
  ServiceHandle
    { findActiveDrivers = pure [Fixtures.defaultDriver],
      findRidesByStartTimeBuffer = \fromTime timeBuffer statuses -> pure [],
      fetchDriversStats = \driverId quantity ->
        pure [Fixtures.defaultDriverStats]
    }

limit :: Integer
limit = 100

time :: UTCTime
time = Fixtures.defaultTime

runTests :: TestTree
runTests =
  testGroup
    "active_drivers endpoint tests"
    [ successfulCaseWithInfo,
      successfulCaseWithNoRides,
      successfulCaseWithNoDrivers,
      successfulCaseWithDriverOnTrip
    ]

successfulCaseWithInfo :: TestTree
successfulCaseWithInfo =
  testCase "Successful case with information" $
    handleGetAvailableDriversInfo handle time limit @?= pure expectedResponse
  where
    expectedResponse =
      API.ActiveDriversResponse
        { active_drivers = [API.DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 2, earnings_over_time = 200.0}]
        }

successfulCaseWithNoRides :: TestTree
successfulCaseWithNoRides =
  testCase "Successful case with drivers without completed rides" $
    handleGetAvailableDriversInfo handleCase time limit @?= pure expectedResponse
  where
    handleCase = handle {fetchDriversStats = \_ _ -> pure [Fixtures.mkDriverStats "1" 0 0]}
    expectedResponse =
      API.ActiveDriversResponse
        { active_drivers = [API.DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 0, earnings_over_time = 0.0}]
        }

successfulCaseWithNoDrivers :: TestTree
successfulCaseWithNoDrivers =
  testCase "Successful case with no active drivers" $
    handleGetAvailableDriversInfo handleCase time limit @?= pure expectedResponse
  where
    handleCase = handle {findActiveDrivers = pure [], fetchDriversStats = \_ _ -> pure []}
    expectedResponse =
      API.ActiveDriversResponse
        { active_drivers = []
        }

successfulCaseWithDriverOnTrip :: TestTree
successfulCaseWithDriverOnTrip =
  testCase "Should successfully filter drivers on a trip" $
    handleGetAvailableDriversInfo handleCase time limit @?= pure expectedResponse
  where
    handleCase =
      handle
        { findRidesByStartTimeBuffer = \_ _ _ -> pure [Fixtures.defaultProductInstance {PI._status = PI.INPROGRESS}],
          fetchDriversStats = \_ _ -> pure [Fixtures.mkDriverStats "1" 0 0]
        }
    expectedResponse =
      API.ActiveDriversResponse
        { active_drivers = [API.DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 0, earnings_over_time = 0.0}]
        }
