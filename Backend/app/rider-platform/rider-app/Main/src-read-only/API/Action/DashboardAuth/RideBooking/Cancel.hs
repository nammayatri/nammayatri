{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Cancel
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Cancel
import qualified Domain.Action.Dashboard.RideBooking.Cancel
import qualified "this" Domain.Action.UI.Cancel
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

type API = ("rideBooking" :> PostCancelBooking)

type PostCancelBooking = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/CANCEL/POST_CANCEL_BOOKING" :> API.Types.Dashboard.RideBooking.Cancel.PostCancelBooking)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCancelBooking merchantId city

postCancelBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Booking.Booking -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Cancel.CancelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCancelBooking a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Cancel.postCancelBooking a6 a5 a3 a2 a1
