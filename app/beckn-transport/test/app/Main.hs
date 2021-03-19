module Main where

import EulerHS.Prelude
import FareCalculator
import Flow.Allocation
import Flow.RideAPI.StartRide (startRide)
import Test.Tasty

main :: IO ()
main = defaultMain =<< specs

specs :: IO TestTree
specs = do
  let rideAPI = testGroup "Ride API" [startRide]
  let unitTests = testGroup "Unit tests" [flowTests, fareCalculator, allocation, rideAPI]
  return $ testGroup "Tests" [unitTests]

flowTests :: TestTree
flowTests =
  testGroup
    "Flow tests"
    []
