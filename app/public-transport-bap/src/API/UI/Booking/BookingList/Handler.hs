module API.UI.Booking.BookingList.Handler where

import API.UI.Booking.BookingList.Types
import App.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Domain.Booking
import Product.BookingList as BookingListHandler
import Tools.Auth

handler :: FlowServer API
handler = bookingList

bookingList :: PersonId -> Maybe Integer -> Maybe Integer -> FlowHandler BookingListRes
bookingList personId mbLimit mbOffset = withFlowHandlerAPI $ do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  BookingListHandler.bookingListHandler personId limit offset