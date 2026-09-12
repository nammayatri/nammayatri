{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.Registration
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Registration
import qualified Domain.Action.Dashboard.RideBooking.Registration
import qualified "this" Domain.Action.UI.Registration
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.RegistrationToken
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("registration" :> (PostRegistrationAuth :<|> PostRegistrationVerify :<|> PostRegistrationOtpResend :<|> PostRegistrationLogout))

type PostRegistrationAuth = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH" :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationAuth)

type PostRegistrationVerify = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY" :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationVerify)

type PostRegistrationOtpResend =
  ( DashboardUserAuth
      ('APP_BACKEND)
      "RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_OTP_RESEND"
      :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationOtpResend
  )

type PostRegistrationLogout = (DashboardUserAuth ('APP_BACKEND) "RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_LOGOUT" :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationLogout)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRegistrationAuth merchantId city :<|> postRegistrationVerify merchantId city :<|> postRegistrationOtpResend merchantId city :<|> postRegistrationLogout merchantId city

postRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.RideBooking.Registration.CustomerAuthReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthRes)
postRegistrationAuth a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationAuth a4 a3 a1

postRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Domain.Action.UI.Registration.AuthVerifyReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthVerifyRes)
postRegistrationVerify a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationVerify a5 a4 a2 a1

postRegistrationOtpResend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Environment.FlowHandler Domain.Action.UI.Registration.ResendAuthRes)
postRegistrationOtpResend a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationOtpResend a4 a3 a1

postRegistrationLogout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationLogout a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationLogout a4 a3 a1
