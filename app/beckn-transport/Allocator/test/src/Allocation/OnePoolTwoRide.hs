module Allocation.OnePoolTwoRide where

import Allocation.Internal
import Beckn.Types.Id
import qualified Data.Map as Map
import Domain.Action.Allocation
import qualified Domain.Types.Booking as SRB
import EulerHS.Prelude hiding (id)
import Test.Tasty
import Test.Tasty.HUnit

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

booking02Id :: Id SRB.Booking
booking02Id = Id "booking02"

allocationDriverResponseAllocationDriverResponse :: TestTree
allocationDriverResponseAllocationDriverResponse = testCaseSteps "Allocation - DriverResponse - Allocation - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(booking01Id, driverPool1), (booking02Id, driverPool1)]
  addBooking r booking01Id 0
  addBooking r booking02Id 0
  addDriverPool r driverPoolPerRide
  step "First booking allocation request - both drivers notified"
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkFreeNotificationStatus r booking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(booking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  checkFreeNotificationStatus r booking01Id (Id "driver01")
  checkFreeNotificationStatus r booking01Id (Id "driver02")
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkFreeNotificationStatus r booking02Id (Id "driver02")
  step "Second booking request - second driver notified"
  addRequest Allocation r booking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r booking01Id (Id "driver01")
  checkFreeNotificationStatus r booking01Id (Id "driver02")
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkNotificationStatus r booking02Id (Id "driver02") Notified
  step "The second driver agrees - the second booking to him"
  addResponse r booking02Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= [Id "driver02", Id "driver01"]
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(booking02Id, Id "driver02"), (booking01Id, Id "driver01")]
  step "Both bookings are assigned"
  checkRideStatus r booking01Id Assigned
  checkRideStatus r booking02Id Assigned

allocationDriverResponseCancellationAllocationDriverResponse :: TestTree
allocationDriverResponseCancellationAllocationDriverResponse = testCaseSteps "Allocation - DriverResponse - Cancellation - Allocation - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(booking01Id, driverPool1), (booking02Id, driverPool1)]
  addBooking r booking01Id 0
  addBooking r booking02Id 0
  addDriverPool r driverPoolPerRide
  step "Request for allocation of the first booking - notified to both"
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkFreeNotificationStatus r booking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(booking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  step "The first one cancels the booking"
  addRequest Cancellation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= []
  checkRideStatus r booking01Id Cancelled
  step "Request for allocation of the second booking - notified both"
  addRequest Allocation r booking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r booking01Id (Id "driver01")
  checkFreeNotificationStatus r booking01Id (Id "driver02")
  checkNotificationStatus r booking02Id (Id "driver01") Notified
  checkNotificationStatus r booking02Id (Id "driver02") Notified
  step "The first driver agrees - the second booking to him"
  addResponse r booking02Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide3 <- readIORef onRideVar
  onRide3 @?= [Id "driver01"]
  assignments3 <- readIORef assignmentsVar
  assignments3 @?= [(booking02Id, Id "driver01"), (booking01Id, Id "driver01")]
  step "First booking - cancelled, second - assigned"
  checkRideStatus r booking01Id Cancelled
  checkRideStatus r booking02Id Assigned

twoAllocationTwoDriverResponse :: TestTree
twoAllocationTwoDriverResponse = testCaseSteps "Allocation - Allocation - DriverResponse - DriverResponse" $ \step -> do
  r@Repository {..} <- initRepository
  let driverPool1 = [Id "driver01", Id "driver02"]
      driverPoolPerRide = Map.fromList [(booking01Id, driverPool1), (booking02Id, driverPool1)]
  addBooking r booking01Id 0
  addBooking r booking02Id 0
  addDriverPool r driverPoolPerRide
  step "Requests for allocation of the both booking - both drivers are notified about first"
  addRequest Allocation r booking01Id
  addRequest Allocation r booking02Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver01") Notified
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkFreeNotificationStatus r booking02Id (Id "driver02")
  step "The first driver agrees - the first booking to him"
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments1 <- readIORef assignmentsVar
  assignments1 @?= [(booking01Id, Id "driver01")]
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  void $ process (handle r) org1 numRequestsToProcess
  checkFreeNotificationStatus r booking01Id (Id "driver01")
  checkFreeNotificationStatus r booking01Id (Id "driver02")
  checkFreeNotificationStatus r booking02Id (Id "driver01")
  checkNotificationStatus r booking02Id (Id "driver02") Notified
  step "The second driver agrees - the second booking to him"
  addResponse r booking02Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= [Id "driver02", Id "driver01"]
  assignments2 <- readIORef assignmentsVar
  assignments2 @?= [(booking02Id, Id "driver02"), (booking01Id, Id "driver01")]
  step "Both bookings are assigned"
  checkRideStatus r booking01Id Assigned
  checkRideStatus r booking02Id Assigned

onePoolTwoRide :: TestTree
onePoolTwoRide =
  testGroup
    "Two bookings for two drivers"
    [ allocationDriverResponseAllocationDriverResponse,
      allocationDriverResponseCancellationAllocationDriverResponse,
      twoAllocationTwoDriverResponse
    ]
