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
bookingList personId mbLimit mbOffset mbBookingStatus = withFlowHandlerAPI $ do
  BookingList.bookingListHandler personId mbLimit mbOffset mbBookingStatus
