{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Allocation.Reassignment where

import Allocation.Internal
import qualified Data.Map as Map
import Domain.Action.Allocation
import qualified Domain.Types.Booking as SRB
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import SharedLogic.DriverPool.Types
import Test.Tasty
import Test.Tasty.HUnit

booking01Id :: Id SRB.Booking
booking01Id = Id "booking01"

booking02Id :: Id SRB.Booking
booking02Id = Id "booking02"

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPool2 :: [Id Driver]
driverPool2 = [Id "driver01", Id "driver03"]

driverPoolPerRide :: Map (Id SRB.Booking, PoolBatchNum) [Id Driver]
driverPoolPerRide = Map.fromList [((booking01Id, 0), driverPool1), ((booking02Id, 0), driverPool2)]

reassignmentRide :: TestTree
reassignmentRide = testCase "Reassignment booking after cancellation" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  addResponse r booking01Id (Id "driver01") Accept
  void $ process (handle r) org1 numRequestsToProcess
  addRequest Cancellation r booking01Id --this won't cancell assigned ride, this request'll be skipped by allocator
  void $ process (handle r) org1 numRequestsToProcess
  updateBooking r booking01Id SRB.AWAITING_REASSIGNMENT
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  checkNotificationStatus r booking01Id (Id "driver02") Notified
  addResponse r booking01Id (Id "driver02") Accept
  void $ process (handle r) org1 numRequestsToProcess
  assignments <- readIORef assignmentsVar
  assignments @?= [(booking01Id, Id "driver02"), (booking01Id, Id "driver01")]
  checkRideStatus r booking01Id SRB.TRIP_ASSIGNED
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
  checkRideStatus r booking02Id SRB.TRIP_ASSIGNED
  onRide <- readIORef onRideVar
  onRide @?= [Id "driver01"]

reassignment :: TestTree
reassignment =
  testGroup
    "Reassignment after cancellation"
    [ reassignmentRide,
      reassignmentDriver
    ]
