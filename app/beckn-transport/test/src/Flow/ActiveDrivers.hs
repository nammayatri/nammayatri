module Flow.ActiveDrivers (runTests) where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Ratio ((%))
import Data.Time (nominalDay)
import EulerHS.Prelude
import qualified Fixtures
import Product.DriverInformation (ServiceHandle (..), handleGetAvailableDriversInfo)
import Test.Tasty
import Test.Tasty.HUnit
import Types.API.DriverInformation (ActiveDriversResponse (..), DriverInformation (..))

handle :: ServiceHandle Identity
handle =
  ServiceHandle
    { findActiveDrivers = pure [Fixtures.defaultDriver],
      findRidesByStartTimeBuffer = \fromTime timeBuffer statuses -> pure [],
      getCurrentTime = pure Fixtures.defaultTime,
      fetchDriversStats = \driverId ->
        pure [Fixtures.defaultDriverStats]
    }

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
    handleGetAvailableDriversInfo handle @?= pure expectedResponse
  where
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = [DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 2, earnings_over_time = 200.0}]
        }

successfulCaseWithNoRides :: TestTree
successfulCaseWithNoRides =
  testCase "Successful case with drivers without completed rides" $
    handleGetAvailableDriversInfo handleCase @?= pure expectedResponse
  where
    handleCase = handle {fetchDriversStats = \_ -> pure [Fixtures.mkDriverStats "1" 0 0]}
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = [DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 0, earnings_over_time = 0.0}]
        }

successfulCaseWithNoDrivers :: TestTree
successfulCaseWithNoDrivers =
  testCase "Successful case with no active drivers" $
    handleGetAvailableDriversInfo handleCase @?= pure expectedResponse
  where
    handleCase = handle {findActiveDrivers = pure [], fetchDriversStats = \_ -> pure []}
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = []
        }

successfulCaseWithDriverOnTrip :: TestTree
successfulCaseWithDriverOnTrip =
  testCase "Should successfully filter drivers on a trip" $
    handleGetAvailableDriversInfo handleCase @?= pure expectedResponse
  where
    handleCase =
      handle
        { findRidesByStartTimeBuffer = \_ _ _ -> pure [Fixtures.defaultProductInstance {PI._status = PI.INPROGRESS}],
          fetchDriversStats = \_ -> pure [Fixtures.mkDriverStats "1" 0 0]
        }
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = [DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 0, earnings_over_time = 0.0}]
        }
