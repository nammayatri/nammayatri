module Domain.Action.Dashboard.RideBooking.Booking
  ( postBookingStatus,
    getBookingList,
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
import SharedLogic.Merchant (findMerchantByShortId)

postBookingStatus ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Booking.Booking ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow Domain.Types.Booking.API.BookingAPIEntity
postBookingStatus merchantShortId _opCity bookingId personId = do
  m <- findMerchantByShortId merchantShortId
  Domain.Action.UI.Booking.bookingStatus bookingId (personId, m.id)

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
