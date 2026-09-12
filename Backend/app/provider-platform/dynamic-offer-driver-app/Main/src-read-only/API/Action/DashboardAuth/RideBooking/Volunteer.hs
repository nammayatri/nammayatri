{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Volunteer
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Volunteer
import qualified Domain.Action.Dashboard.RideBooking.Volunteer
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

type API = ("volunteer" :> (GetVolunteerBooking :<|> PostVolunteerAssignStartOtpRide))

type GetVolunteerBooking = (DashboardUserAuth ('DRIVER_OFFER_BPP) "PROVIDER_RIDE_BOOKING/VOLUNTEER/GET_VOLUNTEER_BOOKING" :> API.Types.Dashboard.RideBooking.Volunteer.GetVolunteerBooking)

type PostVolunteerAssignStartOtpRide =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/VOLUNTEER/POST_VOLUNTEER_ASSIGN_START_OTP_RIDE"
      :> API.Types.Dashboard.RideBooking.Volunteer.PostVolunteerAssignStartOtpRide
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVolunteerBooking merchantId city :<|> postVolunteerAssignStartOtpRide merchantId city

getVolunteerBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Volunteer.BookingInfoResponse)
getVolunteerBooking a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Volunteer.getVolunteerBooking a4 a3 a1

postVolunteerAssignStartOtpRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.RideBooking.Volunteer.AssignCreateAndStartOtpRideAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerAssignStartOtpRide a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Volunteer.postVolunteerAssignStartOtpRide a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
