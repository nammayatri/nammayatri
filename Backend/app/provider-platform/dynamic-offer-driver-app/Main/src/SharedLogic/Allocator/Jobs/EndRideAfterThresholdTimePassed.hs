{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.Allocator.Jobs.EndRideAfterThresholdTimePassed where

import qualified Domain.Action.UI.Ride.EndRide as RideEnd
import qualified Domain.Types.Ride as DRide
import Environment (AppEnv)
import Kernel.Prelude hiding (handle)
import Kernel.Types.Error
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Error (fromMaybeM)
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified Storage.Queries.Ride as QRide

endRideAfterThresholdTimePassed :: Job 'EndRideAfterThresholdTimePassed -> FlowR AppEnv ExecutionResult
endRideAfterThresholdTimePassed Job {jobInfo} = do
  let jobData = jobInfo.jobData
  let rideId = jobData.rideId
  let merchantId = jobData.merchantId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist $ show rideId)
  when (ride.status == DRide.INPROGRESS) $
    void $
      RideEnd.buildEndRideHandle merchantId >>= (\hndl -> RideEnd.scheduleBasedEndRide hndl rideId (RideEnd.ScheduledBasedEndRideReq rideId))
  return Complete
