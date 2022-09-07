module API.UI.Registration
  ( DRegistration.AuthReq (..),
    DRegistration.AuthRes (..),
    DRegistration.ResendAuthRes,
    DRegistration.AuthVerifyReq (..),
    DRegistration.AuthVerifyRes (..),
    API,
    handler,
  )
where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified Domain.Types.RegistrationToken as SRT
import Servant hiding (throwError)
import Utils.Auth (TokenAuth)

---- Registration Flow ------
type API =
  "auth"
    :> ( ReqBody '[JSON] DRegistration.AuthReq
           :> Post '[JSON] DRegistration.AuthRes
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] DRegistration.AuthVerifyReq
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> "resend"
             :> Post '[JSON] DRegistration.ResendAuthRes
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

auth :: DRegistration.AuthReq -> FlowHandler DRegistration.AuthRes
auth = withFlowHandlerAPI . DRegistration.auth

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DRegistration.verify tokenId

resend :: Id SR.RegistrationToken -> FlowHandler DRegistration.ResendAuthRes
resend = withFlowHandlerAPI . DRegistration.resend

logout :: Id SP.Person -> FlowHandler APISuccess
logout personId = withFlowHandlerAPI . withPersonIdLogTag personId $ DRegistration.logout personId
