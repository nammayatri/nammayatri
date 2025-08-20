{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Ride
  ( API.Types.Dashboard.RideBooking.Ride.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Ride.API)
handler merchantId city = postRideStart merchantId city :<|> postRideEnd merchantId city :<|> getRideCurrentActiveRide merchantId city :<|> postRideCancel merchantId city :<|> postRideBookingWithVehicleNumberAndPhone merchantId city

postRideStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.StartRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideStart a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideStart a4 a3 a2 a1

postRideEnd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.EndRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideEnd a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideEnd a4 a3 a2 a1

getRideCurrentActiveRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler (Kernel.Types.Id.Id Dashboard.Common.Ride))
getRideCurrentActiveRide a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.getRideCurrentActiveRide a3 a2 a1

postRideCancel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.Dashboard.RideBooking.Ride.CancelRideReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRideCancel a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideCancel a4 a3 a2 a1

postRideBookingWithVehicleNumberAndPhone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneReq -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Ride.BookingWithVehicleAndPhoneRes)
postRideBookingWithVehicleNumberAndPhone a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Ride.postRideBookingWithVehicleNumberAndPhone a3 a2 a1
