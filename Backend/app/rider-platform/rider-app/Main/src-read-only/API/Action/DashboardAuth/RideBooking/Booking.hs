{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Booking
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Booking
import qualified Domain.Action.Dashboard.RideBooking.Booking
import qualified "this" Domain.Action.UI.Booking
import qualified "this" Domain.Types.Booking
import qualified "this" Domain.Types.Booking.API
import qualified Domain.Types.BookingStatus
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("booking" :> (PostBookingStatus :<|> GetBookingBooking :<|> GetBookingList :<|> GetBookingAgentL1List :<|> GetBookingAgentL2List))

type PostBookingStatus = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS" :> API.Types.Dashboard.RideBooking.Booking.PostBookingStatus)

type GetBookingBooking = API.Types.Dashboard.RideBooking.Booking.GetBookingBooking

type GetBookingList = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_LIST" :> API.Types.Dashboard.RideBooking.Booking.GetBookingList)

type GetBookingAgentL1List = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_AGENT_L1_LIST" :> API.Types.Dashboard.RideBooking.Booking.GetBookingAgentL1List)

type GetBookingAgentL2List = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_AGENT_L2_LIST" :> API.Types.Dashboard.RideBooking.Booking.GetBookingAgentL2List)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postBookingStatus merchantId city :<|> getBookingBooking merchantId city :<|> getBookingList merchantId city :<|> getBookingAgentL1List merchantId city :<|> getBookingAgentL2List merchantId city

postBookingStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Domain.Types.Booking.API.BookingAPIEntity)
postBookingStatus a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Booking.postBookingStatus a5 a4 a2 a1

getBookingBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (API.Types.Dashboard.RideBooking.Booking.BookingSearchType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Types.Booking.API.BookingAPIEntity)
getBookingBooking a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Booking.getBookingBooking a5 a4 a3 a2 a1

getBookingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.BookingStatus.BookingStatus) -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListRes)
getBookingList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Booking.getBookingList a8 a7 a5 a4 a3 a2 a1

getBookingAgentL1List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Domain.Types.BookingStatus.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListRes)
getBookingAgentL1List a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Booking.getBookingAgentL1List a9 a8 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)) a6 a5 a4 a3 a2 a1

getBookingAgentL2List :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (EulerHS.Prelude.Integer) -> Kernel.Prelude.Maybe (Domain.Types.BookingStatus.BookingStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler Domain.Action.UI.Booking.BookingListRes)
getBookingAgentL2List a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Booking.getBookingAgentL2List a9 a8 a6 a5 a4 a3 a2 a1
