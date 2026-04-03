{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.RideBooking.DriverRegistration 
( API.Types.Dashboard.RideBooking.DriverRegistration.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.RideBooking.DriverRegistration
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import qualified Kernel.Types.APISuccess
import qualified API.Types.Dashboard.RideBooking.DriverRegistration



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.DriverRegistration.API)
handler merchantId city = postDriverRegistrationAuth merchantId city :<|> postDriverRegistrationVerify merchantId city
postDriverRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> Environment.FlowHandler Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes)
postDriverRegistrationAuth a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationAuth a3 a2 a1
postDriverRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverRegistrationVerify a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.DriverRegistration.postDriverRegistrationVerify a6 a5 a4 a3 a2 a1



