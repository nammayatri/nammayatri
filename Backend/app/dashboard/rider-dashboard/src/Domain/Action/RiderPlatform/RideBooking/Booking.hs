module Domain.Action.RiderPlatform.RideBooking.Booking
  ( postBookingStatus,
    getBookingList,
    getBookingBooking,
    getBookingAgentL1List,
    getBookingAgentL2List,
  )
where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" Domain.Action.UI.Booking
import qualified "rider-app" Domain.Types.Booking
import qualified "rider-app" Domain.Types.Booking.API
import qualified "beckn-spec" Domain.Types.BookingStatus
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant
import Data.Time

postBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow Domain.Types.Booking.API.BookingAPIEntity)
postBookingStatus merchantShortId opCity apiTokenInfo rideBookingId customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.bookingDSL.postBookingStatus) rideBookingId customerId)

getBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus -> Environment.Flow Domain.Action.UI.Booking.BookingListRes)
getBookingList merchantShortId opCity apiTokenInfo customerId limit offset onlyActive status = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.bookingDSL.getBookingList) customerId limit offset onlyActive status

getBookingBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.Flow Domain.Types.Booking.API.BookingAPIEntity)
getBookingBooking merchantShortId opCity bookingOtp = do
  let checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.bookingDSL.getBookingBooking) bookingOtp

getBookingAgentL1List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Domain.Types.BookingStatus.BookingStatus) -> Kernel.Prelude.Maybe UTCTime -> Kernel.Prelude.Maybe UTCTime -> Environment.Flow Domain.Action.UI.Booking.BookingListRes)
getBookingAgentL1List merchantShortId opCity apiTokenInfo limit offset status fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.bookingDSL.getBookingAgentL1List) (Just apiTokenInfo.personId.getId) limit offset status fromDate toDate

getBookingAgentL2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Domain.Types.BookingStatus.BookingStatus) -> Kernel.Prelude.Maybe UTCTime -> Kernel.Prelude.Maybe UTCTime -> Environment.Flow Domain.Action.UI.Booking.BookingListRes)
getBookingAgentL2List merchantShortId opCity apiTokenInfo limit offset status fromDate toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.bookingDSL.getBookingAgentL2List) limit offset status fromDate toDate
