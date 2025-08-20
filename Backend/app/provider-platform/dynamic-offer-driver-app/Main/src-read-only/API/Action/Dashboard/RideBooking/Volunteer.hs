{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Volunteer
  ( API.Types.Dashboard.RideBooking.Volunteer.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Volunteer.API)
handler merchantId city = getVolunteerBooking merchantId city :<|> postVolunteerAssignStartOtpRide merchantId city

getVolunteerBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Volunteer.BookingInfoResponse)
getVolunteerBooking a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Volunteer.getVolunteerBooking a3 a2 a1

postVolunteerAssignStartOtpRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.RideBooking.Volunteer.AssignCreateAndStartOtpRideAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerAssignStartOtpRide a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Volunteer.postVolunteerAssignStartOtpRide a3 a2 a1
