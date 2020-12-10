module Logic.ActiveDrivers (runTests) where

import Beckn.Types.App (PersonId (..))
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
      getCurrentTime = pure Fixtures.defaultTime,
      getDriverRidesInPeriod = \driverId fromTime toTime -> do
        pure [Fixtures.defaultProductInstance, Fixtures.defaultProductInstance]
    }

runTests :: TestTree
runTests =
  testGroup
    "active_drivers endpoint tests"
    [ successfulCaseWithRides,
      successfulCaseWithNoRides,
      successfulCaseWithNoDrivers
    ]

successfulCaseWithRides :: TestTree
successfulCaseWithRides =
  testCase "Successful case with rides" $
    execute handle @?= pure expectedResponse
  where
    expectedResponse =
      [DriverInformation {driver_id = PersonId "1", completed_rides_over_24h = 2, earnings_over_24h = 200.0}]

successfulCaseWithNoRides :: TestTree
successfulCaseWithNoRides =
  testCase "Successful case with no completed rides" $
    execute handleCase @?= pure expectedResponse
  where
    handleCase = handle {getDriverRidesInPeriod = \_ _ _ -> pure []}
    expectedResponse =
      [DriverInformation {driver_id = PersonId "1", completed_rides_over_24h = 0, earnings_over_24h = 0.0}]

successfulCaseWithNoDrivers :: TestTree
successfulCaseWithNoDrivers =
  testCase "Successful case with no active drivers" $
    execute handleCase @?= pure expectedResponse
  where
    handleCase = handle {findActiveDrivers = pure [], getDriverRidesInPeriod = \_ _ _ -> pure []}
    expectedResponse = []
