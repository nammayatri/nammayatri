module API.UI.Booking.BookingList.Handler where

import API.UI.Booking.BookingList.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Action.UI.BookingList as BookingList
import Domain.Types.Booking
import Environment
import Tools.Auth

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> FlowHandler BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI $ do
  BookingList.bookingListHandler personId mbLimit mbOffset
