{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking.Registration
  ( API.Types.Dashboard.RideBooking.Registration.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.RideBooking.Registration.API)
handler merchantId city = postRegistrationAuth merchantId city :<|> postRegistrationVerify merchantId city :<|> postRegistrationOtpResend merchantId city :<|> postRegistrationLogout merchantId city

postRegistrationAuth :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.RideBooking.Registration.CustomerAuthReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthRes)
postRegistrationAuth a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationAuth a3 a2 a1

postRegistrationVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Domain.Action.UI.Registration.AuthVerifyReq -> Environment.FlowHandler Domain.Action.UI.Registration.AuthVerifyRes)
postRegistrationVerify a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationVerify a4 a3 a2 a1

postRegistrationOtpResend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> Environment.FlowHandler Domain.Action.UI.Registration.ResendAuthRes)
postRegistrationOtpResend a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationOtpResend a3 a2 a1

postRegistrationLogout :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationLogout a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.RideBooking.Registration.postRegistrationLogout a3 a2 a1
