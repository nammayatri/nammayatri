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

import qualified BecknV2.OnDemand.Tags as Beckn
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
  | StopArrivedBuildReq DStopArrivedBuildReq
  | EditDestinationUpdate DEditDestinationUpdateReq
  | TollCrossedBuildReq DTollCrossedBuildReq

data DErrorObject = DErrorObject
  { errorCode :: Text,
    errorMessage :: Text
  }

data DStopArrivedBuildReq = DStopArrivedBuildReq
  { bookingDetails :: BookingDetails,
    taggings :: Maybe Beckn.Taggings
  }

data DEstimateRepetitionReq = DEstimateRepetitionReq
  { bookingDetails :: BookingDetails,
    estimateId :: Id DEst.Estimate,
    cancellationSource :: SBCR.CancellationSource,
    taggings :: Maybe Beckn.Taggings
  }

data DQuoteRepetitionReq = DQuoteRepetitionReq
  { bookingDetails :: BookingDetails,
    newBookingId :: Id DRB.Booking,
    cancellationSource :: SBCR.CancellationSource,
    taggings :: Maybe Beckn.Taggings
  }

data DNewMessageReq = DNewMessageReq
  { bookingDetails :: BookingDetails,
    message :: Text,
    taggings :: Maybe Beckn.Taggings
  }

data DSafetyAlertReq = DSafetyAlertReq
  { bookingDetails :: BookingDetails,
    reason :: Text,
    taggings :: Maybe Beckn.Taggings
  }

data DEditDestinationUpdateReq = DEditDestinationUpdateReq
  { bookingDetails :: BookingDetails,
    bookingUpdateReqDetails :: DBUR.BookingUpdateRequest,
    newDestination :: Maybe DL.Location,
    currentLocation :: Maybe Maps.LatLong,
    updateType :: UpdateType,
    taggings :: Maybe Beckn.Taggings
  }

data DTollCrossedBuildReq = DTollCrossedBuildReq
  { bookingDetails :: BookingDetails,
    taggings :: Maybe Beckn.Taggings
  }

data UpdateType = SOFT_UPDATE | CONFIRM_UPDATE
  deriving (Show)
