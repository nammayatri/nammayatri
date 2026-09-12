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

type API = ("driver" :> (PostDriverRegistrationAuth :<|> PostDriverRegistrationVerify))

type PostDriverRegistrationAuth =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_AUTH"
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationAuth
  )

-- Public shape: mbFleet/fleetOwnerId come from the session.
type PostDriverRegistrationVerify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP)
      "PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY"
      :> API.Types.Dashboard.RideBooking.DriverRegistration.PostDriverRegistrationVerify
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverRegistrationAuth merchantId city :<|> postDriverRegistrationVerify merchantId city

postDriverRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverRegistrationAuth a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationAuth a4 a3 a1

postDriverRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationVerify a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  mbFleet <- Tools.Auth.DashboardUserAuth.requestorFleetFlag a3
  Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationVerify a5 a4 a2 mbFleet (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) a1
