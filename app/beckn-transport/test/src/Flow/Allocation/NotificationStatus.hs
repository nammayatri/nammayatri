module Flow.Allocation.NotificationStatus where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.App

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1)]

checkNotificationStatuses :: TestTree
checkNotificationStatuses = testCaseSteps "Check NotificationStatus" $ \step -> do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  step "All Notified"
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  step "Driver01 Rejected"
  addResponse r rideBooking01Id (Id "driver01") Reject
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Rejected
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  step "Driver01 Ignored"
  threadDelay 1200000
  addDriverInPool r rideBooking01Id (Id "driver03")
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver02") Ignored
  step "Driver03 No NotificationStatus"
  addResponse r rideBooking01Id (Id "driver03") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(rideBooking01Id, Id "driver03")]
  checkRideStatus r rideBooking01Id Assigned
  checkFreeNotificationStatus r rideBooking01Id (Id "driver03")
