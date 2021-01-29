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
    [testOrder]

testOrder :: TestTree
testOrder =
  testCase "Test ordering" $
    (getNearestDrivers pickupPoint 5000 Nothing <&> map (_getPersonId . fst))
      @@?= ["closest_driver", "furthest_driver"]

testFiltrationByOrganization :: TestTree
testFiltrationByOrganization =
  testCase "Test filtration by organization" $
    (getNearestDrivers pickupPoint 5000 orgId <&> map (_getPersonId . fst))
      @@?= ["furthest_driver"]
  where
    orgId = Just $ OrganizationId "org1"

testInRadius :: TestTree
testInRadius =
  testCase "Test radius filtration" $
    (getNearestDrivers pickupPoint 800 Nothing <&> map (_getPersonId . fst))
      @@?= ["closest_driver"]

pickupPoint = LatLong 12.994927 77.596386
