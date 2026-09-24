{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking.DriverRegistration
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.DriverRegistration
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration
import qualified Domain.Action.DashboardAuth.RideBooking.DriverRegistration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (PostDriverRegistrationAuth :<|> PostDriverRegistrationVerify))

type PostDriverRegistrationAuth =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP
      "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_AUTH"
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationAuth
  )

type PostDriverRegistrationVerify =
  ( DashboardUserAuth
      'DRIVER_OFFER_BPP
      "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY"
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationVerify
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverRegistrationAuth merchantId city :<|> postDriverRegistrationVerify merchantId city

postDriverRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverRegistrationAuth a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_AUTH" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationAuth a4 a3 a1
    )

postDriverRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationVerify a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.DashboardAuth.RideBooking.DriverRegistration.postDriverRegistrationVerify a5 a4 a3 a2 a1
    )
