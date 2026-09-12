{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Ride
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Ride
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.RideBooking.Ride
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("ride" :> (PostRideStart :<|> PostRideEnd :<|> GetRideCurrentActiveRide :<|> PostRideCancel :<|> PostRideBookingWithVehicleNumberAndPhone))

type PostRideStart = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_START" :> API.Types.Dashboard.RideBooking.Ride.PostRideStart)

type PostRideEnd = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_END" :> API.Types.Dashboard.RideBooking.Ride.PostRideEnd)

type GetRideCurrentActiveRide = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/RIDE/GET_RIDE_CURRENT_ACTIVE_RIDE" :> API.Types.Dashboard.RideBooking.Ride.GetRideCurrentActiveRide)

type PostRideCancel = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_CANCEL" :> API.Types.Dashboard.RideBooking.Ride.PostRideCancel)

type PostRideBookingWithVehicleNumberAndPhone =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE"
      :> API.Types.Dashboard.RideBooking.Ride.PostRideBookingWithVehicleNumberAndPhone
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRideStart merchantId city :<|> postRideEnd merchantId city :<|> getRideCurrentActiveRide merchantId city :<|> postRideCancel merchantId city :<|> postRideBookingWithVehicleNumberAndPhone merchantId city

postRideStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.StartRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideStart a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideStart a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

postRideEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> (Kernel.Types.Id.Id Dashboard.Common.Ride) -> API.Types.Dashboard.RideBooking.Ride.EndRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideEnd a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideEnd a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

getRideCurrentActiveRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler (Kernel.Types.Id.Id Dashboard.Common.Ride))
getRideCurrentActiveRide a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.getRideCurrentActiveRide a4 a3 a1

postRideCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.CancelRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancel a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideCancel a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1

postRideBookingWithVehicleNumberAndPhone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneRes)
postRideBookingWithVehicleNumberAndPhone a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideBookingWithVehicleNumberAndPhone a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
