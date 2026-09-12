{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Booking
  ( API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Booking
import qualified Dashboard.Common.Booking
import qualified Domain.Action.Dashboard.Booking
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("booking" :> (PostBookingCancelAllStuck :<|> PostBookingSyncMultiple))

type PostBookingCancelAllStuck =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK"
      :> API.Types.RiderPlatform.Management.Booking.PostBookingCancelAllStuck
  )

type PostBookingSyncMultiple = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE" :> API.Types.RiderPlatform.Management.Booking.PostBookingSyncMultiple)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postBookingCancelAllStuck merchantId city :<|> postBookingSyncMultiple merchantId city

postBookingCancelAllStuck :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Booking.StuckBookingsCancelReq -> Environment.FlowHandler Dashboard.Common.Booking.StuckBookingsCancelRes)
postBookingCancelAllStuck a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Booking.postBookingCancelAllStuck a4 a3 a1

postBookingSyncMultiple :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.Common.Booking.MultipleBookingSyncReq -> Environment.FlowHandler Dashboard.Common.Booking.MultipleBookingSyncResp)
postBookingSyncMultiple a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Booking.postBookingSyncMultiple a4 a3 a1
