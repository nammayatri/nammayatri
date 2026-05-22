module API.Dashboard.EmailVerification where

import qualified Domain.Action.Dashboard.EmailVerification as DEV
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant
import Storage.Beam.BeamFlow
import Tools.Auth

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

handler :: BeamFlow' => FlowServer API
handler =
  sendEmailVerificationOtp
    :<|> verifyEmailOtp

sendEmailVerificationOtp :: BeamFlow' => TokenInfo -> DEV.EmailOtpSendReq -> FlowHandler APISuccess
sendEmailVerificationOtp token = withFlowHandlerAPI' . DEV.sendEmailVerificationOtp token

verifyEmailOtp :: BeamFlow' => TokenInfo -> DEV.EmailOtpVerifyReq -> FlowHandler APISuccess
verifyEmailOtp token = withFlowHandlerAPI' . DEV.verifyEmailOtp token
