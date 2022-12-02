module Main where

import EulerHS.Prelude
import Flow.RideAPI.CancelRide (cancelRide)
import Flow.RideAPI.EndRide (endRideTests)
import Flow.RideAPI.StartRide (startRide)
import OneWayFareCalculator
import RentalFareCalculator
import Test.Tasty

main :: IO ()
main = do
  specs >>= defaultMain

--  wrapTests $ \appEnv -> defaultMain $ locationUpdatesTree appEnv

specs :: IO TestTree
specs = do
  let rideAPI = testGroup "Ride API" [startRide, endRideTests, cancelRide]
  return $
    testGroup
      "Unit tests"
      [ fareCalculator,
        rentalFareCalculator,
        rideAPI
      ]
