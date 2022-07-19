module Main where

import EulerHS.Prelude
import Flow.RideAPI.CancelRide (cancelRide)
import Flow.RideAPI.EndRide (endRideTests)
import Flow.RideAPI.StartRide (startRide)
import qualified LocationUpdates as LocUpd
import OneWayFareCalculator
import RentalFareCalculator
import Test.Tasty

main :: IO ()
main = do
  LocUpd.wrapTests (specs >=> defaultMain)

--  wrapTests $ \appEnv -> defaultMain $ locationUpdatesTree appEnv

specs :: LocUpd.AppEnv -> IO TestTree
specs appEnv = do
  let rideAPI = testGroup "Ride API" [startRide, endRideTests, cancelRide]
  return $
    testGroup
      "Unit tests"
      [ fareCalculator,
        rentalFareCalculator,
        rideAPI,
        LocUpd.locationUpdatesTree appEnv
      ]
