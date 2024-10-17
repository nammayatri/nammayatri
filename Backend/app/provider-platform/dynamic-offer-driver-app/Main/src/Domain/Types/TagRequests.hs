{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.TagRequests where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Prelude

data CancelRideTagData = CancelRideTagData
  { ride :: DRide.Ride,
    booking :: DBooking.Booking,
    cancellationReason :: DBCR.BookingCancellationReason,
    callAtemptByDriver :: Bool,
    isDestinationEdited :: Bool,
    currentTime :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data EndRideTagData = EndRideTagData
  { ride :: DRide.Ride,
    booking :: DBooking.Booking
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SearchTagData = SearchTagData
  { searchRequest :: DSR.SearchRequest,
    area :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
  }
  deriving (Generic, Show, ToJSON)
