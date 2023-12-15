{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Booking.BookingList.Handler where

import API.UI.Booking.BookingList.Types
import Domain.Action.UI.BookingList as BookingList
import Domain.Types.Booking
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Auth

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> Maybe BookingStatus -> FlowHandler BookingListRes
bookingList personId mbLimit mbOffset mbBookingStatus = withFlowHandlerAPI' $ do
  BookingList.bookingListHandler personId mbLimit mbOffset mbBookingStatus
