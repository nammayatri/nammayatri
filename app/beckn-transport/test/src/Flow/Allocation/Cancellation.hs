module Flow.Allocation.Cancellation where

import Beckn.Types.Id
import qualified Data.Map as Map
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import qualified Types.API.RideBooking as RideBooking
import Types.App
import qualified Types.Storage.RideBooking as SRB

rideBooking01Id :: Id SRB.RideBooking
rideBooking01Id = Id "rideBooking01"

rideBooking02Id :: Id SRB.RideBooking
rideBooking02Id = Id "rideBooking02"

rideBooking03Id :: Id SRB.RideBooking
rideBooking03Id = Id "rideBooking03"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]

driverPoolPerRide :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide = Map.fromList [(rideBooking01Id, driverPool1)]

driverPool3 :: [Id Driver]
driverPool3 = [Id "driver01", Id "driver02", Id "driver03"]

driverPool4 :: [Id Driver]
driverPool4 = [Id "driver05", Id "driver07", Id "driver08"]

driverPoolPerRide1 :: Map (Id SRB.RideBooking) [Id Driver]
driverPoolPerRide1 = Map.fromList [(rideBooking01Id, driverPool3), (rideBooking02Id, driverPool4)]

cancellationBeforeAssignment :: TestTree
cancellationBeforeAssignment = testCase "Cancellation before assignment" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= []
  checkRideStatus r rideBooking01Id Cancelled

cancellationAfterAssignment :: TestTree
cancellationAfterAssignment = testCase "Cancellation after assignment" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r rideBooking01Id (Id "driver01") RideBooking.ACCEPT
  void $ process (handle r) org1 numRequestsToProcess
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  checkRideStatus r rideBooking01Id Assigned
  addRequest Cancellation r rideBooking01Id
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= []
  checkRideStatus r rideBooking01Id Cancelled
  assignments <- readIORef assignmentsVar
  assignments @?= [(Id {getId = "rideBooking01"}, Id {getId = "driver01"})]
  checkFreeNotificationStatus r rideBooking01Id (Id "driver01")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver02")
  checkFreeNotificationStatus r rideBooking01Id (Id "driver03")
  updateRideBooking r rideBooking01Id AwaitingReassignment
  checkRideStatus r rideBooking01Id AwaitingReassignment

cancellationOnReallocationsCountExceedLimit :: TestTree
cancellationOnReallocationsCountExceedLimit = testCase "Cancellation on reallocations count exceed limit" $ do
  r@Repository {..} <- initRepository
  addRideBooking r rideBooking01Id 0
  addRideBooking r rideBooking02Id 3
  addRideBooking r rideBooking03Id 22
  addDriverPool r driverPoolPerRide1
  addRequest Allocation r rideBooking01Id
  addRequest Allocation r rideBooking02Id
  addRequest Allocation r rideBooking03Id
  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r rideBooking01Id Confirmed
  checkRideStatus r rideBooking02Id Cancelled
  checkRideStatus r rideBooking03Id Cancelled

cancellation :: TestTree
cancellation =
  testGroup
    "Cancellation"
    [ cancellationBeforeAssignment,
      cancellationAfterAssignment,
      cancellationOnReallocationsCountExceedLimit
    ]
