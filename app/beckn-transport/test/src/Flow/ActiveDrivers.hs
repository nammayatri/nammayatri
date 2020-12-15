module Flow.ActiveDrivers (runTests) where

import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Time (nominalDay)
import EulerHS.Prelude hiding (Handle)
import qualified Fixtures
import Product.DriversInformation (Handle (..), execute)
import Test.Tasty
import Test.Tasty.HUnit
import Types.API.DriversInformation (ActiveDriversResponse (..), DriverInformation (..))

handle :: Handle Identity
handle =
  Handle
    { findActiveDrivers = pure [Fixtures.defaultDriver],
      findRidesByStartTimeBuffer = \fromTime timeBuffer statuses -> pure [],
      getCurrentTime = pure Fixtures.defaultTime,
      getDriverRidesInPeriod = \driverId fromTime toTime ->
        pure [Fixtures.defaultProductInstance, Fixtures.defaultProductInstance]
    }

runTests :: TestTree
runTests =
  testGroup
    "active_drivers endpoint tests"
    [ successfulCaseWithRides,
      successfulCaseWithNoRides,
      successfulCaseWithNoDrivers,
      successfulCaseWithDriverOnTrip
    ]

successfulCaseWithRides :: TestTree
successfulCaseWithRides =
  testCase "Successful case with rides" $
    execute handle @?= pure expectedResponse
  where
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = [DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 2, earnings_over_time = 200.0}]
        }

successfulCaseWithNoRides :: TestTree
successfulCaseWithNoRides =
  testCase "Successful case with no completed rides" $
    execute handleCase @?= pure expectedResponse
  where
    handleCase = handle {getDriverRidesInPeriod = \_ _ _ -> pure []}
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = [DriverInformation {driver_id = PersonId "1", completed_rides_over_time = 0, earnings_over_time = 0.0}]
        }

successfulCaseWithNoDrivers :: TestTree
successfulCaseWithNoDrivers =
  testCase "Successful case with no active drivers" $
    execute handleCase @?= pure expectedResponse
  where
    handleCase = handle {findActiveDrivers = pure [], getDriverRidesInPeriod = \_ _ _ -> pure []}
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = []
        }

successfulCaseWithDriverOnTrip :: TestTree
successfulCaseWithDriverOnTrip =
  testCase "Successful case with driver on trip" $
    execute handleCase @?= pure expectedResponse
  where
    handleCase =
      handle
        { findRidesByStartTimeBuffer = \_ _ _ -> pure [Fixtures.defaultProductInstance {PI._status = PI.INPROGRESS}],
          getDriverRidesInPeriod = \_ _ _ -> pure []
        }
    expectedResponse =
      ActiveDriversResponse
        { time = nominalDay,
          active_drivers = []
        }
