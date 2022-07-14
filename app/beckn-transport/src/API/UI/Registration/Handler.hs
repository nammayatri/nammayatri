module API.UI.Registration.Handler (API, handler) where

import API.UI.Registration.Types
import App.Types
import Beckn.Types.APISuccess
import Beckn.Types.Id
import qualified Domain.Action.UI.Registration as DReg
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import EulerHS.Prelude hiding (id)
import Servant hiding (throwError)
import Utils.Auth (TokenAuth)
import Utils.Common

type API =
  "auth"
    :> ( ReqBody '[JSON] AuthReq
           :> Post '[JSON] AuthRes
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] AuthVerifyReq
             :> Post '[JSON] AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> "resend"
             :> Post '[JSON] ResendAuthRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  auth
    :<|> verify
    :<|> resend
    :<|> logout

auth :: AuthReq -> FlowHandler AuthRes
auth = withFlowHandlerAPI . DReg.auth

verify :: Id SR.RegistrationToken -> AuthVerifyReq -> FlowHandler AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DReg.verify tokenId

resend :: Id SR.RegistrationToken -> FlowHandler ResendAuthRes
resend = withFlowHandlerAPI . DReg.resend

logout :: Id SP.Person -> FlowHandler APISuccess
logout = withFlowHandlerAPI . DReg.logout
