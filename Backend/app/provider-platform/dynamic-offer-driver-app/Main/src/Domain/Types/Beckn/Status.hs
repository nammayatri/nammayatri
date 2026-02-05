{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Types.Beckn.Status where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.Beckn.Common as Common

data OnStatusBuildReq
  = NewBookingBuildReq DNewBookingBuildReq
  | BookingReallocationBuildReq DBookingReallocationBuildReq
  | RideAssignedReq Common.DRideAssignedReq
  | RideStartedReq Common.DRideStartedReq
  | RideCompletedReq Common.DRideCompletedReq
  | BookingCancelledReq Common.DBookingCancelledReq

data DStatusReq = StatusReq
  { bookingId :: Maybe (Id DBooking.Booking),
    transactionId :: Text
  }

data DStatusRes = DStatusRes
  { transporter :: DM.Merchant,
    booking :: DBooking.Booking,
    info :: OnStatusBuildReq
  }

type BookingReallocationInfo = BookingCancelledInfo

data BookingCancelledInfo = BookingCancelledInfo
  { cancellationSource :: DBCR.CancellationSource,
    cancellationReasonCode :: Maybe Text
  }

newtype DNewBookingBuildReq = DNewBookingBuildReq
  { bookingId :: Id DBooking.Booking
  }

data DBookingReallocationBuildReq = DBookingReallocationBuildReq
  { bookingReallocationInfo :: BookingReallocationInfo,
    bookingDetails :: Common.BookingDetails
  }

data ScheduledInfo = ScheduledInfo
  { routeDistance :: Maybe Meters,
    dropLocation :: Maybe LatLong
  }
  deriving (Generic, Show)
