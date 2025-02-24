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

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Location as DL
import Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Id
import SharedLogic.Beckn.Common as Reexport

data OnUpdateBuildReq
  = ScheduledRideAssignedBuildReq DRideAssignedReq
  | RideAssignedBuildReq DRideAssignedReq
  | RideStartedBuildReq DRideStartedReq
  | RideCompletedBuildReq DRideCompletedReq
  | BookingCancelledBuildReq DBookingCancelledReq
  | DriverArrivedBuildReq DDriverArrivedReq
  | EstimateRepetitionBuildReq DEstimateRepetitionReq
  | QuoteRepetitionBuildReq DQuoteRepetitionReq
  | NewMessageBuildReq DNewMessageReq
  | SafetyAlertBuildReq DSafetyAlertReq
  | PhoneCallRequestBuildReq DPhoneCallRequestReq
  | PhoneCallCompletedBuildReq DPhoneCallCompletedReq
  | StopArrivedBuildReq DStopArrivedBuildReq
  | EditDestinationUpdate DEditDestinationUpdateReq
  | TollCrossedBuildReq DTollCrossedBuildReq
  | DriverReachedDestinationBuildReq DDriverReachedDestinationReq
  | RideEstimatedEndTimeRangeBuildReq DRideEstimatedEndTimeRangeReq
  | ParcelImageUploadedBuildReq DParcelImageUploadedReq

data DErrorObject = DErrorObject
  { errorCode :: Text,
    errorMessage :: Text
  }

newtype DParcelImageUploadedReq = DParcelImageUploadedReq
  { bookingDetails :: BookingDetails
  }

newtype DStopArrivedBuildReq = DStopArrivedBuildReq
  { bookingDetails :: BookingDetails
  }

data DEstimateRepetitionReq = DEstimateRepetitionReq
  { bookingDetails :: BookingDetails,
    estimateId :: Id DEst.Estimate,
    cancellationSource :: SBCR.CancellationSource
  }

data DQuoteRepetitionReq = DQuoteRepetitionReq
  { bookingDetails :: BookingDetails,
    newBookingId :: Id DRB.Booking,
    cancellationSource :: SBCR.CancellationSource
  }

data DNewMessageReq = DNewMessageReq
  { bookingDetails :: BookingDetails,
    message :: Text
  }

newtype DPhoneCallRequestReq = DPhoneCallRequestReq
  { bookingDetails :: BookingDetails
  }

newtype DPhoneCallCompletedReq = DPhoneCallCompletedReq
  { bookingDetails :: BookingDetails
  }

data DSafetyAlertReq = DSafetyAlertReq
  { bookingDetails :: BookingDetails,
    reason :: Text
  }

data DEditDestinationUpdateReq = DEditDestinationUpdateReq
  { bookingDetails :: BookingDetails,
    bookingUpdateReqDetails :: DBUR.BookingUpdateRequest,
    newDestination :: Maybe DL.Location,
    currentLocation :: Maybe Maps.LatLong,
    updateType :: UpdateType
  }

newtype DTollCrossedBuildReq = DTollCrossedBuildReq
  { bookingDetails :: BookingDetails
  }

data UpdateType = SOFT_UPDATE | CONFIRM_UPDATE
  deriving (Show)

data DDriverReachedDestinationReq = DDriverReachedDestinationReq
  { bookingDetails :: BookingDetails,
    destinationArrivalTime :: Maybe UTCTime
  }

newtype DRideEstimatedEndTimeRangeReq = DRideEstimatedEndTimeRangeReq
  { bookingDetails :: BookingDetails
  }
