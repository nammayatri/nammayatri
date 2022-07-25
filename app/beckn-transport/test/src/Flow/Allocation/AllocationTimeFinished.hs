module Flow.Allocation.AllocationTimeFinished where

import Allocator.Domain.Action.Allocation
import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.Booking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Test.Tasty
import Test.Tasty.HUnit
import Types.App

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPoolPerRide :: Map (Id SRB.Booking) [Id Driver]
driverPoolPerRide = Map.fromList [(booking01Id, driverPool1)]

allocationTimeFinished :: TestTree
allocationTimeFinished = testCase "AllocationTimeFinished" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  threadDelay 3400000
  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r booking01Id Cancelled
