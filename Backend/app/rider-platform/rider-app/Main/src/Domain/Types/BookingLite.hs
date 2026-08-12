{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.BookingLite where

import Domain.Types.Booking (Booking)
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id

-- Booking-side lite reads use Storage.Queries.QueriesExtra.BookingLite.BookingLite
-- (a KV-cached read model whose FromTType' skips location hydration). Only the
-- ride-side lite projection needs a hand-rolled row type:

data RideLiteRow = RideLiteRow
  { rideId :: Id DR.Ride,
    bookingId :: Id Booking,
    totalFare :: Maybe HighPrecMoney, -- app's "computedPrice"
    cancellationChargesOnCancel :: Maybe HighPrecMoney,
    cancellationFeeStatus :: Maybe DR.CancellationFeeStatus
  }
