module Flow.Allocation.OnePoolTwoRide where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

rideBooking02Id :: Id SRB.RideBooking
rideBooking02Id = Id "rideBooking02"

allocationDriverResponseAllocationDriverResponse :: TestTree
allocationDriverResponseAllocationDriverResponse = testCaseSteps "Allocation - DriverResponse - Allocation - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool1)]
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 0
  addDriverPool r driverPoolPerRide
  step "First booking allocation request - both drivers notified"
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r rideBooking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(rideBooking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  checkFreeNotificationStatus r rideBooking01Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver02")
  step "Second booking request - second driver notified"
  addRequest Allocation r rideBooking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r rideBooking01Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkNotificationStatus r rideBooking02Id (Id "driver02") Notified
  step "The second driver agrees - the second booking to him"
  addResponse r rideBooking02Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= [Id "driver02", Id "driver01"]
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(rideBooking02Id, Id "driver02"), (rideBooking01Id, Id "driver01")]
  step "Both bookings are assigned"
  checkRideStatus r rideBooking01Id Assigned
  checkRideStatus r rideBooking02Id Assigned

allocationDriverResponseCancellationAllocationDriverResponse :: TestTree
allocationDriverResponseCancellationAllocationDriverResponse = testCaseSteps "Allocation - DriverResponse - Cancellation - Allocation - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool1)]
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 0
  addDriverPool r driverPoolPerRide
  step "Request for allocation of the first booking - notified to both"
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r rideBooking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(rideBooking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  step "The first one cancels the booking"
  addRequest Cancellation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= []
  checkRideStatus r rideBooking01Id Cancelled
  step "Request for allocation of the second booking - notified both"
  addRequest Allocation r rideBooking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r rideBooking01Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02")
  checkNotificationStatus r rideBooking02Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking02Id (Id "driver02") Notified
  step "The first driver agrees - the second booking to him"
  addResponse r rideBooking02Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide3 <- readIORef onRideVar
  onRide3 @?= [Id "driver01"]
  assignments3 <- readIORef assignmentsVar
  assignments3 @?= [(rideBooking02Id, Id "driver01"), (rideBooking01Id, Id "driver01")]
  step "First booking - cancelled, second - assigned"
  checkRideStatus r rideBooking01Id Cancelled
  checkRideStatus r rideBooking02Id Assigned

twoAllocationTwoDriverResponse :: TestTree
twoAllocationTwoDriverResponse = testCaseSteps "Allocation - Allocation - DriverResponse - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1), (rideBooking02Id, driverPool1)]
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 0
  addDriverPool r driverPoolPerRide
  step "Requests for allocation of the both booking - both drivers are notified about first"
  addRequest Allocation r rideBooking01Id
  addRequest Allocation r rideBooking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r rideBooking01Id (Id "driver01") Notified
  checkNotificationStatus r rideBooking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r rideBooking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(rideBooking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r rideBooking01Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02")
  checkFreeNotificationStatus r rideBooking02Id (Id "driver01")
  checkNotificationStatus r rideBooking02Id (Id "driver02") Notified
  step "The second driver agrees - the second booking to him"
  addResponse r rideBooking02Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= [Id "driver02", Id "driver01"]
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(rideBooking02Id, Id "driver02"), (rideBooking01Id, Id "driver01")]
  step "Both bookings are assigned"
  checkRideStatus r rideBooking01Id Assigned
  checkRideStatus r rideBooking02Id Assigned

onePoolTwoRide :: TestTree
onePoolTwoRide =
  testGroup
    "Two bookings for two drivers"
    [ allocationDriverResponseAllocationDriverResponse,
      allocationDriverResponseCancellationAllocationDriverResponse,
      twoAllocationTwoDriverResponse
    ]
