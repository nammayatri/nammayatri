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

import qualified Domain.Action.UI.Registration as DRegistration
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "auth"
    :> ( ReqBody '[JSON] DRegistration.AuthReq
           :> Header "x-bundle-version" Version
           :> Header "x-client-version" Version
           :> Post '[JSON] DRegistration.AuthRes
           :<|> Capture "authId" (Id SR.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] DRegistration.AuthVerifyReq
             :> Post '[JSON] DRegistration.AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SR.RegistrationToken)
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

auth :: DRegistration.AuthReq -> Maybe Version -> Maybe Version -> FlowHandler DRegistration.AuthRes
auth req mbBundleVersionText = withFlowHandlerAPI . DRegistration.auth req mbBundleVersionText

verify :: Id SR.RegistrationToken -> DRegistration.AuthVerifyReq -> FlowHandler DRegistration.AuthVerifyRes
verify tokenId = withFlowHandlerAPI . DRegistration.verify tokenId

resend :: Id SR.RegistrationToken -> FlowHandler DRegistration.ResendAuthRes
resend = withFlowHandlerAPI . DRegistration.resend

logout :: Id SP.Person -> FlowHandler APISuccess
logout = withFlowHandlerAPI . DRegistration.logout
