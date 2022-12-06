module Allocation.Cancellation where

import Allocation.Internal
import Beckn.Types.Id
import qualified Data.Map as Map
import Domain.Action.Allocation
import qualified Domain.Types.Booking as SRB
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)
import Test.Tasty
import Test.Tasty.HUnit

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

booking02Id :: Id SRB.Booking
booking02Id = Id "booking02"

booking03Id :: Id SRB.Booking
booking03Id = Id "booking03"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02", Id "driver03"]

driverPoolPerRide :: Map (Id SRB.Booking) [Id Driver]
driverPoolPerRide = Map.fromList [(booking01Id, driverPool1)]

driverPool3 :: [Id Driver]
driverPool3 = [Id "driver01", Id "driver02", Id "driver03"]

driverPool4 :: [Id Driver]
driverPool4 = [Id "driver05", Id "driver07", Id "driver08"]

driverPoolPerRide1 :: Map (Id SRB.Booking) [Id Driver]
driverPoolPerRide1 = Map.fromList [(booking01Id, driverPool3), (booking02Id, driverPool4)]

cancellationBeforeAssignment :: TestTree
cancellationBeforeAssignment = testCase "Cancellation before assignment" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= []
  checkRideStatus r booking01Id SRB.CANCELLED

cancellationAfterAssignment :: TestTree
cancellationAfterAssignment = testCase "Cancellation after assignment" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  onRide1 <- readIORef onRideVar
  onRide1 @?= [Id "driver01"]
  checkRideStatus r booking01Id SRB.TRIP_ASSIGNED
  addRequest Cancellation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  onRide2 <- readIORef onRideVar
  onRide2 @?= []
  checkRideStatus r booking01Id SRB.CANCELLED
  assignments <- readIORef assignmentsVar
  assignments @?= [(Id {getId = "booking01"}, Id {getId = "driver01"})]
  checkFreeNotificationStatus r booking01Id (Id "driver01")
  checkFreeNotificationStatus r booking01Id (Id "driver02")
  checkFreeNotificationStatus r booking01Id (Id "driver03")
  updateBooking r booking01Id SRB.AWAITING_REASSIGNMENT
  checkRideStatus r booking01Id SRB.AWAITING_REASSIGNMENT

cancellationOnReallocationsCountExceedLimit :: TestTree
cancellationOnReallocationsCountExceedLimit = testCase "Cancellation on reallocations count exceed limit" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addBooking r booking02Id 3
  addBooking r booking03Id 22
  addDriverPool r driverPoolPerRide1
  addRequest Allocation r booking01Id
  addRequest Allocation r booking02Id
  addRequest Allocation r booking03Id
  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r booking01Id SRB.CONFIRMED
  checkRideStatus r booking02Id SRB.CANCELLED
  checkRideStatus r booking03Id SRB.CANCELLED

cancellation :: TestTree
cancellation =
  testGroup
    "Cancellation"
    [ cancellationBeforeAssignment,
      cancellationAfterAssignment,
      cancellationOnReallocationsCountExceedLimit
    ]
