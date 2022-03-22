module Flow.Allocation.TwoAllocations where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.RideBooking as RideBooking

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

rideBooking02Id :: Id SRB.RideBooking
rideBooking02Id = Id "rideBooking02"

twoAllocations :: TestTree
twoAllocations = testCase "Two allocations" $ do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]
      driverPool2 = [Id "driver05", Id "driver07", Id "driver08"]
      driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool2)]
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  addRequest Allocation r rideBooking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking02Id (Id "driver05") Notified
  addResponse r rideBooking01Id (Id "driver01") RideBooking.REJECT
  addResponse r rideBooking02Id (Id "driver05") RideBooking.REJECT
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver02") RideBooking.REJECT
  addResponse r rideBooking02Id (Id "driver07") RideBooking.REJECT
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver03") RideBooking.ACCEPT
  addResponse r rideBooking02Id (Id "driver08") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(rideBooking02Id, Id "driver08"), (rideBooking01Id, Id "driver03")]
  checkRideStatus r rideBooking01Id Assigned
  checkRideStatus r rideBooking02Id Assigned
