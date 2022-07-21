module Flow.Allocation.TwoAllocations where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.Booking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

booking02Id :: Id SRB.Booking
booking02Id = Id "booking02"

twoAllocations :: TestTree
twoAllocations = testCase "Two allocations" $ do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]
      driverPool2 = [Id "driver05", Id "driver07", Id "driver08"]
      driverPoolPerRide = Map.fromList [(booking01Id, driverPool1), (booking02Id, driverPool2)]
  addBooking r booking01Id 0
  addBooking r booking02Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  addRequest Allocation r booking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking02Id (Id "driver05") Notified
  addResponse r booking01Id (Id "driver01") Reject
  addResponse r booking02Id (Id "driver05") Reject
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver02") Reject
  addResponse r booking02Id (Id "driver07") Reject
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver03") Accept
  addResponse r booking02Id (Id "driver08") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(booking02Id, Id "driver08"), (booking01Id, Id "driver03")]
  checkRideStatus r booking01Id Assigned
  checkRideStatus r booking02Id Assigned
