module API.Dashboard.EmailVerification where

import qualified Domain.Action.Dashboard.EmailVerification as DEV
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Common
import Servant
import Tools.Auth.Dashboard
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow, withDashboardDbFlowHandlerAPI)

type API =
  "user"
    :> "email"
    :> ( "sendOtp"
           :> DashboardAuth 'DASHBOARD_USER
           :> ReqBody '[JSON] DEV.EmailOtpSendReq
           :> Post '[JSON] APISuccess
           :<|> "verifyOtp"
             :> DashboardAuth 'DASHBOARD_USER
             :> ReqBody '[JSON] DEV.EmailOtpVerifyReq
             :> Post '[JSON] APISuccess
       )

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler =
  sendEmailVerificationOtp
    :<|> verifyEmailOtp

sendEmailVerificationOtp :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DEV.EmailOtpSendReq -> FlowHandlerR r APISuccess
sendEmailVerificationOtp token = withDashboardDbFlowHandlerAPI . DEV.sendEmailVerificationOtp token

verifyEmailOtp :: DashboardLoginFlow (FlowR r) r => TokenInfo -> DEV.EmailOtpVerifyReq -> FlowHandlerR r APISuccess
verifyEmailOtp token = withDashboardDbFlowHandlerAPI . DEV.verifyEmailOtp token
