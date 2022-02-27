{-# LANGUAGE OverloadedLabels #-}

module Flow.Allocation.AllocationTimeFinished where

import Beckn.Types.Id
import qualified Data.Map as Map
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.App
import qualified Types.Storage.RideBooking as SRB

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1)]

allocationTimeFinished :: TestTree
allocationTimeFinished = testCase "AllocationTimeFinished" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  threadDelay 3400000
  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r rideBooking01Id Cancelled
