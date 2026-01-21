{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Booking
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Booking
import qualified Domain.Action.RiderPlatform.RideBooking.Booking
import qualified "rider-app" Domain.Action.UI.Booking
import qualified "rider-app" Domain.Types.Booking
import qualified "rider-app" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("booking" :> (PostBookingStatus :<|> GetBookingBooking :<|> GetBookingList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postBookingStatus merchantId city :<|> getBookingBooking merchantId city :<|> getBookingList merchantId city

type PostBookingStatus =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.BOOKING / 'API.Types.Dashboard.RideBooking.Booking.POST_BOOKING_STATUS)
      :> API.Types.Dashboard.RideBooking.Booking.PostBookingStatus
  )

type GetBookingBooking = API.Types.Dashboard.RideBooking.Booking.GetBookingBooking

type GetBookingList =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.BOOKING / 'API.Types.Dashboard.RideBooking.Booking.GET_BOOKING_LIST)
      :> API.Types.Dashboard.RideBooking.Booking.GetBookingList
  )

postBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Types.Booking.API.BookingAPIEntity)
postBookingStatus merchantShortId opCity apiTokenInfo rideBookingId customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Booking.postBookingStatus merchantShortId opCity apiTokenInfo rideBookingId customerId

getBookingBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Domain.Types.Booking.API.BookingAPIEntity)
getBookingBooking merchantShortId opCity bookingCode = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Booking.getBookingBooking merchantShortId opCity bookingCode

getBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe EulerHS.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.BookingStatus.BookingStatus -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListRes)
getBookingList merchantShortId opCity apiTokenInfo customerId limit offset onlyActive status = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Booking.getBookingList merchantShortId opCity apiTokenInfo customerId limit offset onlyActive status
