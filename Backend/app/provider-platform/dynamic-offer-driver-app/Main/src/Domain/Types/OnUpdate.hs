{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.OnUpdate
  ( module Domain.Types.OnUpdate,
    module Reexport,
  )
where

import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEst
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.Beckn.Common as Reexport

data OnUpdateBuildReq
  = RideAssignedBuildReq DRideAssignedReq
  | RideStartedBuildReq DRideStartedReq
  | RideCompletedBuildReq DRideCompletedReq
  | BookingCancelledBuildReq DBookingCancelledReq
  | DriverArrivedBuildReq DDriverArrivedReq
  | EstimateRepetitionBuildReq DEstimateRepetitionReq
  | NewMessageBuildReq DNewMessageReq
  | SafetyAlertBuildReq DSafetyAlertReq
  | StopArrivedBuildReq DStopArrivedBuildReq

newtype DStopArrivedBuildReq = DStopArrivedBuildReq
  { bookingDetails :: BookingDetails
  }

data DEstimateRepetitionReq = DEstimateRepetitionReq
  { bookingDetails :: BookingDetails,
    estimateId :: Id DEst.Estimate,
    cancellationSource :: SBCR.CancellationSource
  }

data DNewMessageReq = DNewMessageReq
  { bookingDetails :: BookingDetails,
    message :: Text
  }

data DSafetyAlertReq = DSafetyAlertReq
  { bookingDetails :: BookingDetails,
    reason :: Text
  }
