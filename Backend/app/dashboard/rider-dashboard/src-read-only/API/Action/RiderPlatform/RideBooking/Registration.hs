{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking.Registration
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Registration
import qualified Domain.Action.RiderPlatform.RideBooking.Registration
import qualified "rider-app" Domain.Action.UI.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.RegistrationToken
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("registration" :> (PostRegistrationAuth :<|> PostRegistrationVerify :<|> PostRegistrationOtpResend :<|> PostRegistrationLogout))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRegistrationAuth merchantId city :<|> postRegistrationVerify merchantId city :<|> postRegistrationOtpResend merchantId city :<|> postRegistrationLogout merchantId city

type PostRegistrationAuth =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.REGISTRATION / 'API.Types.Dashboard.RideBooking.Registration.POST_REGISTRATION_AUTH)
      :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationAuth
  )

type PostRegistrationVerify =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.REGISTRATION / 'API.Types.Dashboard.RideBooking.Registration.POST_REGISTRATION_VERIFY)
      :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationVerify
  )

type PostRegistrationOtpResend =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.REGISTRATION / 'API.Types.Dashboard.RideBooking.Registration.POST_REGISTRATION_OTP_RESEND)
      :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationOtpResend
  )

type PostRegistrationLogout =
  ( ApiAuth
      'APP_BACKEND
      'DSL
      ('RIDER_RIDE_BOOKING / 'API.Types.Dashboard.RideBooking.REGISTRATION / 'API.Types.Dashboard.RideBooking.Registration.POST_REGISTRATION_LOGOUT)
      :> API.Types.Dashboard.RideBooking.Registration.PostRegistrationLogout
  )

postRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.RideBooking.Registration.CustomerAuthReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthRes)
postRegistrationAuth merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Registration.postRegistrationAuth merchantShortId opCity apiTokenInfo req

postRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Domain.Action.UI.Registration.AuthVerifyReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthVerifyRes)
postRegistrationVerify merchantShortId opCity apiTokenInfo authId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Registration.postRegistrationVerify merchantShortId opCity apiTokenInfo authId req

postRegistrationOtpResend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Environment.FlowHandler Domain.Action.UI.Registration.ResendAuthRes)
postRegistrationOtpResend merchantShortId opCity apiTokenInfo authId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Registration.postRegistrationOtpResend merchantShortId opCity apiTokenInfo authId

postRegistrationLogout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationLogout merchantShortId opCity apiTokenInfo customerId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.RideBooking.Registration.postRegistrationLogout merchantShortId opCity apiTokenInfo customerId
