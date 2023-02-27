{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Allocation.AllocationTimeFinished where

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

driverPool1 :: [Id Driver]
driverPool1 = [Id "driver01", Id "driver02"]

driverPoolPerRide :: Map (Id SRB.Booking, PoolBatchNum) [Id Driver]
driverPoolPerRide = Map.fromList [((booking01Id, 0), driverPool1)]

allocationTimeFinished :: TestTree
allocationTimeFinished = testCase "AllocationTimeFinished" $ do
  r@Repository {..} <- initRepository
  addBooking r booking01Id 0
  addDriverPool r driverPoolPerRide
  addRequest Allocation r booking01Id
  void $ process (handle r) org1 numRequestsToProcess
  threadDelay 3400000
  void $ process (handle r) org1 numRequestsToProcess
  checkRideStatus r booking01Id SRB.CANCELLED
