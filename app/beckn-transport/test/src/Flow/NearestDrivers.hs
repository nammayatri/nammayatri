module Flow.NearestDrivers (runTests) where

import App.Types
import Beckn.Types.App
import EulerHS.Prelude
import Storage.Queries.Person (getNearestDrivers)
import Test.Tasty
import Test.Tasty.HUnit
import Types.API.Location (LatLong (..))

runFlow :: Flow a -> IO a
runFlow = error "TODO"

infix 1 @@?=

(@@?=) :: Flow a -> a -> Assertion
fa @@?= b = error "TODO"

-- fa @@?= b = (b @?=) <$> runFlow fa

runTests :: TestTree
runTests =
  testGroup
    "Test getNearestDriversFunction"
    [ testOrder,
      testInRadius
    ]

testOrder :: TestTree
testOrder =
  testCase "Test ordering" $
    (getNearestDrivers pickupPoint 5000 org1 <&> map (_getPersonId . fst))
      @@?= ["closest_driver", "furthest_driver"]

testInRadius :: TestTree
testInRadius =
  testCase "Test radius filtration" $
    (getNearestDrivers pickupPoint 800 org1 <&> map (_getPersonId . fst))
      @@?= ["other_driver"]

pickupPoint = LatLong 12.994927 77.596386

org1 = OrganizationId "org1"
