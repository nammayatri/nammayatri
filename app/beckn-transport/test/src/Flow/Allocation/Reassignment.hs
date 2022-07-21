module Flow.Allocation.Reassignment where

import Beckn.Types.Id
import qualified Data.Map as Map
import qualified Domain.Types.Booking as SRB
import EulerHS.Prelude hiding (id)
import Flow.Allocation.Internal
import Services.Allocation.Allocation
import Test.Tasty
import Test.Tasty.HUnit
import Types.App

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

booking02Id :: Id SRB.Booking
booking02Id = Id "booking02"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver01", Id "driver03"]

driverPoolPerRide :: Map (Id SRB.Booking) [Id Driver]
driverPoolPerRide = Map.fromList [(booking01Id, driverPool1), (booking02Id, driverPool2)]

reassignmentRide :: TestTree
reassignmentRide = testCase "Reassignment booking after cancellation" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  updateBooking r booking01Id AwaitingReassignment
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  addResponse r booking01Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(booking01Id, Id "driver02"), (booking01Id, Id "driver01")]
  checkRideStatus r booking01Id Assigned
  onRide <- readIORef onRideVar
  onRide @?= [Id "driver02"]

reassignmentDriver :: TestTree
reassignmentDriver = testCase "Reassignment driver after cancellation" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addBooking r booking02Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Allocation r booking02Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking02Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(booking02Id, Id "driver01"), (booking01Id, Id "driver01")]
  checkRideStatus r booking02Id Assigned
  onRide <- readIORef onRideVar
  onRide @?= [Id "driver01"]

reassignment :: TestTree
reassignment =
  testGroup
    "Reassignment after cancellation"
    [ reassignmentRide,
      reassignmentDriver
    ]
