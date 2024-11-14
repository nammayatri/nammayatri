{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking.Volunteer
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Volunteer
import qualified Domain.Action.ProviderPlatform.RideBooking.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("volunteer" :> (GetVolunteerBooking :<|> PostVolunteerAssignStartOtpRide))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getVolunteerBooking merchantId city :<|> postVolunteerAssignStartOtpRide merchantId city

type GetVolunteerBooking =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.VOLUNTEER / 'API.Types.Dashboard.RideBooking.Volunteer.GET_VOLUNTEER_BOOKING)
      :> API.Types.Dashboard.RideBooking.Volunteer.GetVolunteerBooking
  )

type PostVolunteerAssignStartOtpRide =
  ( ApiAuth
      'DRIVER_OFFER_BPP
      'DSL
      ('PROVIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.VOLUNTEER / 'API.Types.Dashboard.RideBooking.Volunteer.POST_VOLUNTEER_ASSIGN_START_OTP_RIDE)
      :> API.Types.Dashboard.RideBooking.Volunteer.PostVolunteerAssignStartOtpRide
  )

getVolunteerBooking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.RideBooking.Volunteer.BookingInfoResponse)
getVolunteerBooking merchantShortId opCity apiTokenInfo bookingOtp = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Volunteer.getVolunteerBooking merchantShortId opCity apiTokenInfo bookingOtp

postVolunteerAssignStartOtpRide :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.Volunteer.AssignCreateAndStartOtpRideAPIReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerAssignStartOtpRide merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.RideBooking.Volunteer.postVolunteerAssignStartOtpRide merchantShortId opCity apiTokenInfo req
