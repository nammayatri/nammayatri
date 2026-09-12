{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.AddBaggage
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.AddBaggage
import qualified Domain.Action.Dashboard.RideBooking.AddBaggage
import qualified "this" Domain.Types.Booking
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("rideBooking" :> PostAddBaggageConfirm)

type PostAddBaggageConfirm = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/ADD_BAGGAGE/POST_ADD_BAGGAGE_CONFIRM" :> API.Types.Dashboard.RideBooking.AddBaggage.PostAddBaggageConfirm)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postAddBaggageConfirm merchantId city

postAddBaggageConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.RideBooking.AddBaggage.AddBaggageConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAddBaggageConfirm a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.AddBaggage.postAddBaggageConfirm a6 a5 a3 a2 a1
