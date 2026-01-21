module Domain.Action.Dashboard.RideBooking.Booking
  ( postBookingStatus,
    getBookingList,
    getBookingBooking,
  )
where

import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Booking as SQB
import Tools.Error

postBookingStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow Domain.Types.Booking.API.BookingAPIEntity
postBookingStatus merchantShortId _opCity bookingId personId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Booking.bookingStatus bookingId (personId, m.id)

getBookingBooking ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Text ->
  Environment.Flow Domain.Types.Booking.API.BookingAPIEntity
getBookingBooking merchantShortId _opCity bookingCode = do
  apiRateLimitOptions <- asks (.apiRateLimitOptions)
  checkSlidingWindowLimitWithOptions bookingOtpKey apiRateLimitOptions
  m <- findMerchantByShortId merchantShortId
  booking <- SQB.findById (Kernel.Types.Id.Id bookingCode) >>= fromMaybeM (BookingNotFound bookingCode) -- later change it to invoice number or something
  Domain.Action.UI.Booking.bookingStatus booking.id (booking.riderId, m.id)
  where
    bookingOtpKey = "booking-code-" <> bookingCode

getBookingList ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus ->
  Environment.Flow Domain.Action.UI.Booking.BookingListRes
getBookingList merchantShortId _opCity personId mbLimit mbOffset mbOnlyActive bookingStatus = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Booking.bookingList (personId, m.id) mbLimit mbOffset mbOnlyActive bookingStatus Nothing Nothing Nothing []
