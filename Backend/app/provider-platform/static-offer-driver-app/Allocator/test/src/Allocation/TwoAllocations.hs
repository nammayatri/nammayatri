{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Allocation.TwoAllocations where

import Allocation.Internal
import qualified Data.Map as Map
import Domain.Action.Allocation
import qualified Domain.Types.Booking as SRB
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
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
      driverPoolPerRide = Map.fromList [((booking01Id, 0), driverPool1), ((booking02Id, 0), driverPool2)]
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
  checkRideStatus r booking01Id SRB.TRIP_ASSIGNED
  checkRideStatus r booking02Id SRB.TRIP_ASSIGNED
