module Domain.Action.Dashboard.RideBooking.Booking
  ( postBookingStatus,
    getBookingList,
    getBookingBooking,
    getBookingAgentL1List,
    getBookingAgentL2List,
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
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified SharedLogic.DisplayBookingId as DBI
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as SQB
import qualified Storage.Queries.Person as QPerson
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
  -- Try displayBookingId Redis lookup first, then fallback to using bookingCode as bookingId
  mbBookingIdFromDisplay <- DBI.findBookingIdByDisplayId bookingCode
  let bookingId = fromMaybe (Kernel.Types.Id.Id bookingCode) mbBookingIdFromDisplay
  booking <- SQB.findById bookingId >>= fromMaybeM (BookingNotFound bookingCode)
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
  Domain.Action.UI.Booking.bookingList (Just personId, m.id) Nothing False mbLimit mbOffset mbOnlyActive bookingStatus Nothing Nothing Nothing [] Nothing

getBookingAgentL1List ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow Domain.Action.UI.Booking.BookingListRes
getBookingAgentL1List merchantShortId opCity mbAgentId mbLimit mbOffset mbBookingStatus mbCustomerPhoneNo mbFromDate mbToDate = do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let mbFromDateMS = mbFromDate <&> round . utcToMilliseconds
      mbToDateMS = mbToDate <&> round . utcToMilliseconds
  mbPersonId <-
    case mbCustomerPhoneNo of
      Just customerPhoneNo -> do
        mobileNumberHash <- getDbHash customerPhoneNo
        mbPerson <- QPerson.findByMobileNumberAndMerchantId "+91" mobileNumberHash m.id
        return $ mbPerson <&> (.id)
      Nothing -> return Nothing
  Domain.Action.UI.Booking.bookingList (mbPersonId, m.id) mbAgentId True mbLimit mbOffset Nothing mbBookingStatus Nothing mbFromDateMS mbToDateMS [] (Just merchantOperatingCity.id)

getBookingAgentL2List ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe EulerHS.Prelude.Integer ->
  Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Environment.Flow Domain.Action.UI.Booking.BookingListRes
getBookingAgentL2List merchantShortId opCity mbLimit mbOffset mbBookingStatus mbCustomerPhoneNo mbFromDate mbToDate = do
  m <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let mbFromDateMS = mbFromDate <&> round . utcToMilliseconds
      mbToDateMS = mbToDate <&> round . utcToMilliseconds
  mbPersonId <-
    case mbCustomerPhoneNo of
      Just customerPhoneNo -> do
        mobileNumberHash <- getDbHash customerPhoneNo
        mbPerson <- QPerson.findByMobileNumberAndMerchantId "+91" mobileNumberHash m.id
        return $ mbPerson <&> (.id)
      Nothing -> return Nothing
  Domain.Action.UI.Booking.bookingList (mbPersonId, m.id) Nothing True mbLimit mbOffset Nothing mbBookingStatus Nothing mbFromDateMS mbToDateMS [] (Just merchantOperatingCity.id)
